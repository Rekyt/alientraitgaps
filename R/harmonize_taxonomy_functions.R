match_tnrs = function(species_df) {

  TNRS::TNRS(
    species_df,
    sources = c("wcvp", "wfo"),
    classification = "wfo"
  )

}

create_tnrs_df = function(species_df, id_prefix, ...) {

  all_columns = rlang::enquos(...)

  species_df |>
    mutate(id = paste0(id_prefix, "_", row_number()),
           species_name = paste(!!!all_columns)) |>
    select(id, species_name)

}

get_gift_raw_species_df = function(gift_traits) {

  gift_traits |>
    distinct(orig_ID, genus, species_epithet, subtaxon, author) |>
    mutate(
      id = orig_ID,
      # Replace NA with empty strings for easier later concatenation
      across(genus:author, \(x) stringr::str_replace_na(x, "")),
      # Remove introduced double spaces when pasting all parts of species name
      species_name = gsub(
        "  ", " ", paste(genus, species_epithet, subtaxon, author)
      )
    ) |>
    distinct(id, species_name) |>
    mutate(new_id = paste0("gift-", row_number()))

}

get_austraits_taxonomy = function(austraits_species) {

  austraits_species |>
    distinct(taxon_name, binomial)

}

get_bien_taxonomy = function(bien_species) {

  bien_species |>
    distinct(scrubbed_species_binomial, name_submitted)

}

get_gift_taxonomy = function(gift_species) {

  gift_species |>
    distinct(work_ID, work_species)

}

get_try_taxonomy = function(try_harmonized_species) {

  try_harmonized_species |>
    distinct(AccSpeciesID = TRY_AccSpeciesID, MatchedName)

}


get_glonaf_taxonomy = function(glonaf_alien_species) {

  glonaf_alien_species |>
    distinct(taxon_wcvp_id, taxon_orig_id, taxa_accepted) |>
    # Transform accepted taxa into binomial names
    mutate(
      taxa_binomial = stringr::str_extract(taxa_accepted, "^\\w+ [\\w,\\.]+")
    ) |>
    select(-taxa_accepted) |>
    filter(!is.na(taxa_binomial))  # Remove unmatched GloNAF species

}


subset_glonaf_species = function(
    traits_harmonized, glonaf_harmonized, column_name
) {

  traits_harmonized |>
    semi_join(
      glonaf_harmonized,
      dplyr::join_by(!!(column_name) == "taxa_binomial")
    )

}
