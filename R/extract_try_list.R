extract_try_list = function(try_species) {
  try_species %>%
    filter(validEnc(AccSpeciesName),
           grepl(" ", AccSpeciesName, fixed = TRUE)) %>%
    pull(AccSpeciesName)
}

select_most_measured_traits = function(glonaf_species_per_trait, n = 200) {
  glonaf_species_per_trait %>%
    arrange(desc(n_sp)) %>%
    slice_max(n_sp, n = n)
}
