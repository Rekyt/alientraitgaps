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
    function(x, y) full_join(x, y, by = "species"),
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
      !is.na(mean_hii_v2geo), !is.na(mean_GDP_PPP_2015),
      !is.na(mean_access_cities_2015)
    ) %>%
    select(
      species, n_traits, simp_form, n_total, n_total_non_native, mean_hii_v2geo,
      mean_GDP_PPP_2015, mean_access_cities_2015
    ) %>%
    mutate(
      across(n_total:mean_access_cities_2015, ~ as.numeric(scale(.x)))
    ) %>%
    {
      glm(
        n_traits ~ simp_form + n_total + n_total_non_native + mean_hii_v2geo +
          mean_GDP_PPP_2015 + mean_access_cities_2015,
        data = ., family = poisson
      )
    }

}
