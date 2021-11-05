harmonize_austraits_glonaf = function(match_austraits_tnrs, match_glonaf_tnrs) {
  match_austraits_tnrs %>%
    distinct(name_init_austraits        = Name_submitted,
             species_accepted_austraits = Accepted_species) %>%
    inner_join(
      match_glonaf_tnrs %>%
        distinct(name_init_glonaf        = Name_submitted,
                 species_accepted_glonaf = Accepted_species),
      by = c(species_accepted_austraits = "species_accepted_glonaf"))
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
