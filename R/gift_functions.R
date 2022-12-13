# Functions to wrangle GIFT trait data specifically

extract_raw_gift_species = function(gift_all_raw_traits) {
  gift_all_raw_traits %>%
    distinct(orig_ID, genus, species_epithet, author, subtaxon) %>%
    mutate(
      full_name = paste(
      genus, species_epithet,
      ifelse(!is.na(author), author, ""),
      ifelse(!is.na(subtaxon), subtaxon, "")
    )
  )
}

extract_gift_species_names = function(gift_names) {
  gift_names %>%
    select(genus, species_epithet, subtaxon, author) %>%
    mutate(full_name = paste(
      genus, species_epithet,
      ifelse(!is.na(author), author, ""),
      ifelse(!is.na(subtaxon), subtaxon, "")
    )) %>%
    pull(full_name) %>%
    unique()
}

extract_gift_names_with_traits = function(gift_traits_final, gift_names) {
  gift_names %>%
    semi_join(
      gift_traits_final %>%
        distinct(work_ID),
      by = "work_ID"
    )
}

retrieve_all_gift_checklists = function(gift_api, gift_version) {
  GIFT::GIFT_checklist(
    taxon_name = "Tracheophyta",
    complete_taxon = FALSE,
    floristic_group = "all",
    complete_floristic = FALSE,
    geo_type = "All",
    suit_geo = FALSE,
    api = gift_api,
    GIFT_version = gift_version
  )
}

match_taxonomy_checklists_raw = function(
    gift_all_raw_traits, gift_raw_species, gift_raw_list, match_raw_gift_tnrs
) {

  # Original raw names
  original_names = gift_all_raw_traits %>%
    distinct(orig_ID, name_ID, work_ID, genus_ID, work_species)

  # Correspondence between original names and used string to match them
  submitted_names = gift_raw_species %>%
    distinct(orig_ID, full_name) %>%
    inner_join(
      gift_raw_list %>%
        tibble::enframe("other_id", "full_name") %>%
        mutate(ID = paste0("gift-raw-", row_number())) %>%
        select(ID, full_name),
      by = "full_name"
    )

  # Make full correspondence between original names and rematched ones
  match_raw_gift_tnrs %>%
    full_join(submitted_names, by = "ID") %>%
    select(orig_ID, Accepted_species) %>%
    # Add 'work_ID' for prematched names to retrieve in checklists
    full_join(original_names, by = "orig_ID")
}

match_checklist = function(gift_matched_taxonomy, gift_checklists) {
  gift_checklists[[2]] %>%
    full_join(
      gift_matched_taxonomy %>%
        distinct(work_ID, genus_ID, work_species, Accepted_species),
      by = c("work_ID", "genus_ID", "work_species")
    ) %>%
    distinct(
      ref_ID, list_ID, entity_ID, Accepted_species, native, naturalized,
      endemic_list
    )
}

simplify_gift_distribution = function(gift_matched_checklists) {

  # Simplify species status per polygon
  # The code smells but comes from GIFT::GIFT_species_distribution()
  # Couldn't simplify it with case_when() or order functions
  gift_matched_checklists %>%
    filter(Accepted_species != "") %>%
    group_by(entity_ID, Accepted_species) %>%
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
    select(entity_ID, Accepted_species, everything()) %>%
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

harmonize_gift_glonaf = function(match_gift_tnrs, match_glonaf_tnrs) {
  match_gift_tnrs %>%
    distinct(
      gift_id = ID,
      name_init_gift        = Name_submitted,
      species_accepted_gift = Accepted_species
    ) %>%
    inner_join(
      match_glonaf_tnrs %>%
        distinct(
          glonaf_id = ID,
          name_init_glonaf        = Name_submitted,
          species_accepted_glonaf = Accepted_species
        ),
      by = c(species_accepted_gift = "species_accepted_glonaf")
    ) %>%
    filter(species_accepted_gift != "")
}

get_gift_traits_for_glonaf_species = function(
    gift_traits_final, gift_names_traits, harmonized_gift_glonaf,
    gift_traits_meta
) {
  gift_traits_final %>%
    inner_join(
      gift_names_traits %>%
        distinct(work_ID, species),
      by = "work_ID"
    ) %>%
    inner_join(
      harmonized_gift_glonaf %>%
        distinct(species = name_init_gift, species_accepted_gift),
      by = "species"
    ) %>%
    inner_join(
      gift_traits_meta %>%
        distinct(trait_ID = Lvl3, Trait2),
      by = "trait_ID"
    ) %>%
    distinct(species_accepted_gift, Trait2)
}

count_gift_species_per_trait = function(gift_glonaf_traits) {
  gift_glonaf_traits %>%
    dplyr::count(Trait2, sort = TRUE, name = "n_sp")
}

count_gift_trait_per_species = function(gift_glonaf_traits) {
  gift_glonaf_traits %>%
    dplyr::count(species_accepted_gift, sort = TRUE, name = "n_traits")
}
