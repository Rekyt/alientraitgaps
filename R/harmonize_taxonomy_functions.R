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
