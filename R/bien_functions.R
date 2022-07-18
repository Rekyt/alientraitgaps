count_bien_traits_per_species = function(harmonized_try_glonaf) {
  harmonized_try_glonaf %>%
    pull(species_accepted_try) %>%
    unique() %>%
    BIEN::BIEN_trait_traits_per_species()
}

get_bien_traits = function(harmonized_try_glonaf) {
  harmonized_try_glonaf %>%
    pull(species_accepted_try) %>%
    unique() %>%
    BIEN::BIEN_trait_species()
}

count_bien_species_per_trait = function(glonaf_bien_traits_count) {
  glonaf_bien_traits_count %>%
    distinct(species = scrubbed_species_binomial, trait_name) %>%
    filter(!is.na(trait_name)) %>%
    group_by(trait_name) %>%
    summarise(n_sp = n()) %>%
    arrange(desc(n_sp))
}
