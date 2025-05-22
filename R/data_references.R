# Script to gather data references from the trait databases

gather_austraits_references = function(
    austraits, austraits_tnrs, combined_traits_full
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

gather_gift_references = function() {

  gift_sub_traits = combined_traits_full |>
    filter(database == "GIFT") |>
    distinct(species) |>
    inner_join(
      gift_raw_tnrs |>
        distinct(id = ID, species = Accepted_species) |>
        filter(species != ""),
      by = "species"
    )

}
