get_alien_range_size = function(glonaf_species_regions_status) {
  glonaf_species_regions_status %>%
    distinct(species, OBJIDsic) %>%
    count(species, name = "n_alien_regions")
}

get_invasive_range_size = function(glonaf_species_regions_status) {
  glonaf_species_regions_status %>%
    filter(status_name == "invasive") %>%
    distinct(species, OBJIDsic) %>%
    count(species, name = "n_invasive_regions")
}

get_continental_origin = function() {

  glonaf_con = connect_glonaf_db()

  continent_origin = glonaf_con %>%
    tbl("origin_distribution") %>%
    select(species_id, origin_id) %>%
    inner_join(
      tbl(glonaf_con, "origin"),
      by = c(origin_id = "id")
    ) %>%
    collect()

  discon(glonaf_con)

  return(continent_origin)
}

assemble_trait_knowledge_df = function(
    combined_traits, simplified_growth_form, species_socioecovars,
    match_glonaf_tnrs
) {

  all_unified_species = match_glonaf_tnrs %>%
    distinct(species = Accepted_species)

  number_measured_traits = combined_traits %>%
    group_by(species) %>%
    summarise(n_traits = n())

  Reduce(
    function(x, y) inner_join(x, y, by = "species"),
    list(number_measured_traits, simplified_growth_form, species_socioecovars,
         all_unified_species)
  ) %>%
    select(species, simp_form, everything()) %>%
    # Replace problematic NAs by 0s
    mutate(
      across(c(n_traits, n_total:n_unknown), .fns = ~tidyr::replace_na(.x, 0))
    ) %>%
    filter(species != "") %>%
    mutate(
      n_total_non_native = n_total - n_native
    )

}

model_alien_trait_knowledge = function(trait_knowledge_df) {

  trait_knowledge_df %>%
    filter(
      !is.na(mean_hii_v2geo_mean), !is.na(gdp_mean_native),
      !is.na(gdp_mean_non_native), !is.na(mean_access_cities_2015_mean)
    ) %>%
    select(
      species, n_traits, simp_form, n_total, n_total_non_native, n_biomes,
      mean_hii_v2geo_mean, mean_hii_v2geo_sd, gdp_mean_native,
      gdp_mean_non_native, mean_access_cities_2015_mean
    ) %>%
    mutate(
      across(n_total:mean_access_cities_2015_mean, ~ as.numeric(scale(.x)))
    ) %>%
    rename(
      growth_form                   = simp_form,
      total_range_size              = n_total,
      non_native_range_size         = n_total_non_native,
      avg_human_influence_index     = mean_hii_v2geo_mean,
      sd_human_influence_index      = mean_hii_v2geo_sd,
      avg_gdp_over_native_range     = gdp_mean_native,
      avg_gdp_over_non_native_range = gdp_mean_non_native,
      avg_accessibility             = mean_access_cities_2015_mean
    ) %>%
    {
      glmmTMB::glmmTMB(
        n_traits ~ growth_form + total_range_size + non_native_range_size +
          n_biomes + avg_human_influence_index + sd_human_influence_index +
          avg_gdp_over_native_range + avg_gdp_over_non_native_range +
          avg_accessibility,
        family    = glmmTMB::nbinom2,
        ziformula = ~ 0,
        data      = .
      )
    }

}


create_trait_knowledge_table = function(trait_knowledge_model) {

  first_table = as.data.frame(report::report_table(trait_knowledge_model))

  first_table %>%
    select(-df_error, -c(Component:Fit)) %>%
    filter(!(Parameter %in% c("AIC", "AICc", "BIC", "Sigma")),
           !is.na(Parameter)) %>%
    mutate(
      Coeff_95CI = paste0(
        round(Coefficient, 2), " [", round(CI_low, 2), " – ", round(CI_high, 2),
        "]"
      ),
      p_val = case_when(
        is.na(p)  ~ NA_character_,
        p <= 1e-3 ~ "p < 0.001",
        TRUE      ~ as.character(round(p, 3))
      ),
      z = round(z, 1),
      Parameter = case_when(
        Parameter == "(Intercept)" & is.na(z) ~ "Intercept (dispersion)",
        Parameter == "(Intercept)"            ~ "Intercept",
        TRUE ~ tools::toTitleCase(Parameter)
      )
    ) %>%
    select(-Coefficient, -starts_with("CI"), -p) %>%
    select(Parameter, Coeff_95CI, z, p_val) %>%
    mutate(
      Parameter = Parameter %>%
        gsub("Gdp", "GDP", ., fixed = TRUE) %>%
        gsub("Avg", "Average", ., fixed = TRUE) %>%
        gsub("Sd", "Standard Deviation of", ., fixed = TRUE) %>%
        gsub("^n", "Number of", .) %>%
        gsub("Non Native", "Non-native", ., fixed = TRUE)
    ) %>%
    rename(`Coefficient (95% CI)` = Coeff_95CI)

}
