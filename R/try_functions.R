simplify_try_traits = function(try_traits) {

  try_traits |>
    filter(!is.na(TraitID)) |>
    distinct(AccSpeciesID, TraitID, TraitName)

}
