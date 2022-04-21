harmonize_austraits_glonaf = function(match_austraits_tnrs, match_glonaf_tnrs) {
  match_austraits_tnrs %>%
    distinct(name_init_austraits        = Search,
             species_accepted_austraits = Output.Taxon) %>%
    inner_join(
      match_glonaf_tnrs %>%
        distinct(name_init_glonaf        = Search,
                 species_accepted_glonaf = Output.Taxon),
      by = c(species_accepted_austraits = "species_accepted_glonaf"))
}

make_austraits_try_traits_correspond = function(austraits_try_convert) {
  austraits_try_convert %>%
    filter(!is.na(trait_name)) %>%
    select(aus_trait_name = trait_name,
           try_trait_name = `TRY name`,
           try_trait_id   = `TRY number`) %>%
    # Convert list of traits ids from character to actual list of integers
    mutate(
      try_trait_id = purrr::map(
        try_trait_id, function(trait_id_list) {
          if (!is.na(trait_id_list)) {
            # Split list by non words chararacters
            strsplit(trait_id_list, "\\W+") %>%
              .[[1]] %>%
              as.integer()
          } else {
            NA_integer_
          }
        }
      )
    )
}

make_austraits_bien_traits_correspond = function(
  aus_try_convert_df, bien_try_convert_df) {

  tibble::tribble(
    ~bien_trait_name, ~aus_trait_name,
    "plant flowering duration", "flowering_time",
    "plant fruiting duration",  "fruiting_time"
  )
}

get_austraits_traits_for_glonaf_species = function(
  austraits, harmonized_austraits_glonaf
) {
  austraits[["traits"]] %>%
    distinct(taxon_name, trait_name) %>%
    inner_join(harmonized_austraits_glonaf %>%
                 distinct(name_init_austraits, species_accepted_austraits),
               by = c(taxon_name = "name_init_austraits")) %>%
    distinct(species_accepted_austraits, trait_name) %>%
    arrange(species_accepted_austraits, trait_name) %>%
    filter(species_accepted_austraits != "")
}


count_austraits_species_per_trait = function(aus_traits) {
  aus_traits %>%
    dplyr::count(trait_name, sort = TRUE, name = "n_sp")
}

count_austraits_trait_per_species = function(aus_traits) {
  aus_traits %>%
    dplyr::count(species_accepted_austraits, sort = TRUE, name = "n_traits")
}

get_austraits_trait_combinations = function(aus_traits) {
  aus_traits %>%
    dplyr::group_by(species = species_accepted_austraits) %>%
    dplyr::summarise(trait_names = list(trait_name))
}

get_austraits_top_trait_combinations = function(aus_traits, aus_top_traits) {
  aus_traits %>%
    semi_join(aus_top_traits, by = "trait_name") %>%
    get_austraits_trait_combinations()
}
