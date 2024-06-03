simplify_try_traits = function(try_traits) {

  try_traits |>
    filter(!is.na(TraitID)) |>
    distinct(AccSpeciesID, TraitID, TraitName)

}

extract_try_list = function(try_species) {
  try_species %>%
    filter(validEnc(AccSpeciesName),
           grepl(" ", AccSpeciesName, fixed = TRUE)) %>%
    pull(AccSpeciesName)
}
