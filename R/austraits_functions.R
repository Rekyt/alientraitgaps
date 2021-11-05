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
