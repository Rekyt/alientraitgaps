simplify_try_traits = function(try_traits) {

  try_traits |>
    filter(!is.na(TraitID)) |>
    distinct(AccSpeciesID, TraitID, TraitName)

}

match_try_traits_taxonomy = function(full_try_df, try_tnrs) {

  full_try_df |>
    distinct(SpeciesName, TraitID, TraitName) |>
    filter(!is.na(TraitID)) |>
    inner_join(
      try_tnrs |>
        distinct(SpeciesName = Name_submitted, Accepted_species),
      by = "SpeciesName"
    ) |>
    distinct(Accepted_species, TraitID, TraitName)

}
