# Script to gather data references from the trait databases

gather_austraits_references = function(
    austraits, austraits_tnrs, austraits_species_df, combined_traits_full
) {

  aus_species = combined_traits_full |>
    filter(database == "AusTraits") |>
    distinct(species) |>
    right_join(
      austraits_tnrs |>
        distinct(ID, species = Accepted_species) |>
        filter(species != "")
    ) |>
    inner_join(
      austraits_species_df,
      by = join_by(ID == id)
    )

  aus_datasets = austraits$traits |>
    select(dataset_id, taxon_name) |>
    semi_join(
      aus_species, by = join_by(taxon_name == species_name)
    ) |>
    distinct(dataset_id) |>
    pull(dataset_id)

  aus_cites = austraits$sources[aus_datasets]

}

gather_bien_references = function(bien_traits, combined_traits_full) {

  bien_sub_traits = bien_traits |>
    semi_join(
      combined_traits_full |>
        filter(database == "BIEN") |>
        distinct(species),
      by = join_by(scrubbed_species_binomial == species)
    )

  bien_ranges = split(
    1:nrow(bien_sub_traits),
    cut(1:nrow(bien_sub_traits), breaks = 5, labels = FALSE)
  )

  bien_cites = purrr::map(
    bien_ranges,
    \(x_interval) BIEN::BIEN_metadata_citation(
      trait.dataframe = bien_sub_traits[x_interval,]
    ))

  bien_transposed = purrr::transpose(bien_cites)

  bien_final = purrr::map(bien_transposed[1:2], \(x) unique(unlist(x)))
  bien_final$acknowledgements = bien_transposed$acknowledgements

  return(bien_final)

}

gather_gift_references = function(
    gift_raw_tnrs, gift_raw_species_df, gift_raw_traits, combined_traits_full
) {

  gift_sub_traits = combined_traits_full |>
    filter(database == "GIFT") |>
    distinct(species) |>
    inner_join(
      gift_raw_tnrs |>
        distinct(new_id = ID, species = Accepted_species) |>
        filter(species != ""),
      by = "species"
    ) |>
    inner_join(
      gift_raw_species_df, by = "new_id"
    )

  gift_raw_traits |>
    distinct(orig_ID, genus, species_epithet, subtaxon, author, ref_long) |>
    mutate(
      id = orig_ID,
      # Replace NA with empty strings for easier later concatenation
      across(genus:author, \(x) stringr::str_replace_na(x, "")),
      # Remove introduced double spaces when pasting all parts of species name
      species_name = gsub(
        "  ", " ", paste(genus, species_epithet, subtaxon, author)
      )
    ) |>
    distinct(id, species_name, ref_long) |>
    semi_join(gift_sub_traits, by = c("id", "species_name")) |>
    pull(ref_long) |>
    unique()

}

gather_try_references = function(
    combined_traits_full, try_tnrs, try_species_df, try_harmonized_species,
    full_try_df
) {

  try_sub_traits = combined_traits_full |>
    filter(database == "TRY") |>
    distinct(species) |>
    inner_join(
      try_tnrs |>
        distinct(id = ID, species = Accepted_species) |>
        filter(species != ""),
      by = "species"
    ) |>
    distinct(id) |>
    inner_join(
      try_species_df,
      by = "id"
    ) |>
    rename(TRY_SpeciesName = species_name) |>
    inner_join(
      try_harmonized_species |>
        select(TRY_SpeciesName, TRY_AccSpeciesID),
      by = "TRY_SpeciesName"
    )

  try_cites = full_try_df |>
    semi_join(
      try_sub_traits, by = join_by(AccSpeciesID == TRY_AccSpeciesID)
    ) |>
    distinct(Reference) |>
    pull(Reference)

}
