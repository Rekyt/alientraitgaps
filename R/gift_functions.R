# Functions to wrangle GIFT trait data specifically

extract_raw_gift_species = function(gift_all_raw_traits, gift_checklists) {

  traits_raw_species = gift_all_raw_traits %>%
    distinct(orig_ID, genus, species_epithet, author, subtaxon) %>%
    mutate(
      full_name = paste(
      genus, species_epithet,
      ifelse(!is.na(author), author, ""),
      ifelse(!is.na(subtaxon), subtaxon, "")
    )
  )

  checklists_raw_species = gift_checklists[[2]] %>%
    distinct(orig_ID, genus, species_epithet, author, subtaxon) %>%
    mutate(
      full_name = paste(
        genus, species_epithet,
        ifelse(!is.na(author), author, ""),
        ifelse(!is.na(subtaxon), subtaxon, "")
      ) %>%
        trimws(which = "both")
    )

  traits_raw_species %>%
    bind_rows(
      checklists_raw_species
    ) %>%
    distinct(orig_ID, genus, species_epithet, author, subtaxon, full_name)

}



retrieve_all_gift_checklists = function(gift_api, gift_version) {
  GIFT::GIFT_checklists(
    taxon_name = "Tracheophyta",
    complete_taxon = FALSE,
    floristic_group = "all",
    complete_floristic = FALSE,
    geo_type = "All",
    suit_geo = FALSE,
    namesmatched = TRUE,
    api = gift_api,
    GIFT_version = gift_version
  )
}


simplify_gift_distribution = function(gift_checklists) {

  # Simplify species status per polygon
  # The code smells but comes from GIFT::GIFT_species_distribution()
  # Couldn't simplify it with case_when() or order functions
  gift_checklists$checklists %>%
    select(
      entity_ID, work_species, ref_ID, list_ID, native, naturalized,
      endemic_list
    ) |>
    group_by(entity_ID, work_species) %>%
    dplyr::mutate(
      conflict_native       = ifelse(length(unique(native)) > 1, 1, 0),
      conflict_naturalized  = ifelse(length(unique(naturalized)) > 1, 1, 0),
      conflict_endemic_list = ifelse(length(unique(endemic_list)) > 1, 1, 0),
      native = ifelse(
        1 %in% native, 1,
        ifelse(0 %in% native, 0, NA)
      ),
      naturalized = ifelse(
        0 %in% naturalized, 0,
        ifelse(1 %in% naturalized, 1, NA)
      ),
      endemic_list = ifelse(
        0 %in% endemic_list, 0,
        ifelse(1 %in% endemic_list, 1, NA)
      )
    ) %>%
    distinct(native, naturalized, endemic_list, .keep_all = TRUE) %>%
    ungroup() %>%
    select(-ref_ID, -list_ID) %>%
    distinct() %>%
    select(entity_ID, work_species, everything()) %>%
    mutate(status = case_when(
      native == 1 & naturalized == 0     ~ "native",
      native == 1 & is.na(naturalized)   ~ "native",
      native == 0 & is.na(naturalized)   ~ "non-native",
      native == 0 & naturalized == 1     ~ "naturalized",
      native == 0 & naturalized == 0     ~ "non-native",
      is.na(native) & is.na(naturalized) ~ "unknown",
      TRUE                               ~ "unknown"
    ))

}

simplify_gift_traits = function(gift_raw_traits) {

  gift_raw_traits |>
    distinct(work_ID, work_species, trait_ID)

}


match_gift_traits_taxonomy = function(
    gift_raw_traits, gift_raw_tnrs, gift_trait_meta
) {

  gift_raw_traits |>
    distinct(orig_ID = as.character(orig_ID), trait_ID) |>
    inner_join(
      gift_raw_tnrs |>
        distinct(orig_ID = ID, Accepted_species),
      by = "orig_ID"
    ) |>
    distinct(Accepted_species, trait_ID) |>
    inner_join(
      gift_trait_meta |>
        distinct(trait_ID = Lvl3, gift_trait_name = Trait2),
      by = c("trait_ID")
    )

}
