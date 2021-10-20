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
