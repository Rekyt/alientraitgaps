# Functions to wrangle GIFT trait data specifically

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

harmonize_gift_glonaf = function(match_gift_tnrs, match_glonaf_tnrs) {
  match_gift_tnrs %>%
    distinct(
      gift_id = id,
      name_init_gift        = Name_submitted,
      species_accepted_gift = Accepted_species
    ) %>%
    inner_join(
      match_glonaf_tnrs %>%
        distinct(
          glonaf_id = id,
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
