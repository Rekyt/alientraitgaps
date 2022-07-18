extract_try_list = function(try_species) {
  try_species %>%
    filter(validEnc(AccSpeciesName),
           grepl(" ", AccSpeciesName, fixed = TRUE)) %>%
    pull(AccSpeciesName)
}
