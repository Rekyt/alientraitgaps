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
    glonaf_tnrs
) {

  simplified_growth_form = simplified_growth_form |>
    distinct(species = taxon_name, simplified_growth_form)

  all_unified_species = glonaf_tnrs %>%
    filter(Accepted_species != "") |>
    distinct(species = Accepted_species)

  number_measured_traits = combined_traits[[1]] %>%
    mutate(n_traits = purrr::map_int(traits, length)) |>
    select(species, n_traits)

  Reduce(
    function(x, y) inner_join(x, y, by = "species"),
    list(number_measured_traits, simplified_growth_form, species_socioecovars,
         all_unified_species)
  ) %>%
    select(species, simplified_growth_form, everything()) %>%
    full_join(all_unified_species, by = "species") |>
    # Replace problematic NAs by 0s
    mutate(
      across(c(n_traits, n_total:n_unknown), .fns = ~tidyr::replace_na(.x, 0))
    ) %>%
    mutate(
      n_total_non_native = n_total - n_native
    )

}

model_alien_trait_knowledge = function(trait_knowledge_df) {

  trait_knowledge_df = trait_knowledge_df %>%
    filter(
      !is.na(mean_hii_v2geo_mean), !is.na(gdp_mean_native),
      !is.na(gdp_mean_non_native), !is.na(mean_access_cities_2015_mean)
    ) %>%
    select(
      species, n_traits, simplified_growth_form , n_total, n_total_non_native,
      n_biomes, mean_hii_v2geo_mean, mean_hii_v2geo_sd, gdp_mean_native,
      gdp_mean_non_native, mean_access_cities_2015_mean
    ) %>%
    mutate(
      across(n_total:mean_access_cities_2015_mean, ~ as.numeric(scale(.x)))
    ) %>%
    rename(
      growth_form                   = simplified_growth_form,
      total_range_size              = n_total,
      non_native_range_size         = n_total_non_native,
      avg_human_influence_index     = mean_hii_v2geo_mean,
      sd_human_influence_index      = mean_hii_v2geo_sd,
      avg_gdp_over_native_range     = gdp_mean_native,
      avg_gdp_over_non_native_range = gdp_mean_non_native,
      avg_accessibility             = mean_access_cities_2015_mean
    )

  MASS::glm.nb(
    n_traits ~ growth_form + total_range_size + non_native_range_size +
      n_biomes + avg_human_influence_index + sd_human_influence_index +
      avg_gdp_over_native_range + avg_gdp_over_non_native_range +
      avg_accessibility + 0,
    contrasts = list(growth_form = contr.sum),
    data      = trait_knowledge_df
  )

}


model_alien_trait_knowledge_with_intercept = function(trait_knowledge_df) {

  trait_knowledge_df = trait_knowledge_df %>%
    filter(
      !is.na(mean_hii_v2geo_mean), !is.na(gdp_mean_native),
      !is.na(gdp_mean_non_native), !is.na(mean_access_cities_2015_mean)
    ) %>%
    select(
      species, n_traits, simplified_growth_form , n_total, n_total_non_native,
      n_biomes, mean_hii_v2geo_mean, mean_hii_v2geo_sd, gdp_mean_native,
      gdp_mean_non_native, mean_access_cities_2015_mean
    ) %>%
    mutate(
      across(n_total:mean_access_cities_2015_mean, ~ as.numeric(scale(.x)))
    ) %>%
    rename(
      growth_form                   = simplified_growth_form,
      total_range_size              = n_total,
      non_native_range_size         = n_total_non_native,
      avg_human_influence_index     = mean_hii_v2geo_mean,
      sd_human_influence_index      = mean_hii_v2geo_sd,
      avg_gdp_over_native_range     = gdp_mean_native,
      avg_gdp_over_non_native_range = gdp_mean_non_native,
      avg_accessibility             = mean_access_cities_2015_mean
    )

  MASS::glm.nb(
    n_traits ~ growth_form + total_range_size + non_native_range_size +
      n_biomes + avg_human_influence_index + sd_human_influence_index +
      avg_gdp_over_native_range + avg_gdp_over_non_native_range +
      avg_accessibility,
    contrasts = list(growth_form = \(x) contr.treatment(x, base = 2)),
    data      = trait_knowledge_df
  )

}

estimate_phylogenetic_signal = function(
    trait_knowledge_df, trait_knowledge_model, glonaf_tree
) {

  # Get same df as in model
  original_df = trait_knowledge_df %>%
    filter(
      !is.na(mean_hii_v2geo_mean), !is.na(gdp_mean_native),
      !is.na(gdp_mean_non_native), !is.na(mean_access_cities_2015_mean)
    ) %>%
    select(
      species, n_traits, simplified_growth_form , n_total, n_total_non_native,
      n_biomes, mean_hii_v2geo_mean, mean_hii_v2geo_sd, gdp_mean_native,
      gdp_mean_non_native, mean_access_cities_2015_mean
    ) %>%
    mutate(
      across(n_total:mean_access_cities_2015_mean, ~ as.numeric(scale(.x)))
    ) %>%
    rename(
      growth_form                   = simplified_growth_form,
      total_range_size              = n_total,
      non_native_range_size         = n_total_non_native,
      avg_human_influence_index     = mean_hii_v2geo_mean,
      sd_human_influence_index      = mean_hii_v2geo_sd,
      avg_gdp_over_native_range     = gdp_mean_native,
      avg_gdp_over_non_native_range = gdp_mean_non_native,
      avg_accessibility             = mean_access_cities_2015_mean
    )

  # Make smaller df with residuals
  actual_df = original_df |>
    select(species) |>
    mutate(
      species = gsub(" ", "_", species, fixed = TRUE),
      trait_knowledge_resid = resid(trait_knowledge_model)
    )

  new_tree = as(glonaf_tree, "phylo4d")
  phylobase::tipData(new_tree)$trait_resid = tibble::deframe(actual_df)[phylobase::tipLabels(new_tree)]

  # Estimate phylogenetic signal
  trait_knowledge_signal = phylosignal::phyloSignal(
    new_tree, methods = "Lambda", reps = 100
  )

}
