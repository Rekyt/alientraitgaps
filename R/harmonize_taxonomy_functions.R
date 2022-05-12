harmonize_try_glonaf_species = function(match_try_tnrs, match_glonaf_tnrs) {
  # Subset most important columns
  sub_try = match_try_tnrs %>%
    distinct(
      name_init_try        = Name_submitted,
      status_try           = Taxonomic_status,
      name_accepted_try    = Accepted_name,
      species_accepted_try = Accepted_species,
      author_accepted_try  = Author_matched
    )

  sub_glonaf = match_glonaf_tnrs %>%
    distinct(
      name_init_glonaf        = Name_submitted,
      status_glonaf           = Taxonomic_status,
      name_accepted_glonaf    = Accepted_name,
      species_accepted_glonaf = Accepted_species,
      author_accepted_glonaf  = Author_matched
    )

  # Merge TRY and GloNAF accepted names
  sub_try %>%
    filter(species_accepted_try != "") %>%
    inner_join(
      sub_glonaf %>%
        filter(species_accepted_glonaf != ""),
      by = c(species_accepted_try = "species_accepted_glonaf")
    )
}

get_try_ids_from_harmonized_species = function(
  harmonized_try_glonaf, try_species
) {
  harmonized_try_glonaf %>%
    # Getting back TRY species IDs
    inner_join(try_species,
               by = c(name_init_try = "AccSpeciesName"))
}

count_trait_try = function(harmonized_try_glonaf, try_species) {
  harmonized_try_glonaf %>%
    get_try_ids_from_harmonized_species(try_species) %>%
    # Regrouping species with similar harmonized names
    group_by(species_accepted_try) %>%
    summarise(
      observation_number = sum(ObsNum),
      trait_number       = sum(TraitNum),
      measure_number     = sum(MeasNum),
      georef_obs_number  = sum(ObsGRNum),
      georef_measure_number = sum(MeasGRNum))
}

list_all_traits_glonaf = function(harmonized_try_glonaf, try_species, try_df) {
  harmonized_try_glonaf %>%
    # Getting back TRY species IDs
    inner_join(try_species %>%
                 select(AccSpeciesID, AccSpeciesName),
               by = c(name_init_try = "AccSpeciesName")) %>%
    # Add TRY traits
    inner_join(try_df %>%
                 filter(!is.na(TraitID)) %>%
                 collect(), by = "AccSpeciesID")
}

count_species_per_trait = function(glonaf_try_traits_available) {
  glonaf_try_traits_available %>%
    distinct(species_accepted_try, TraitID, TraitName) %>%
    count(TraitName, sort = TRUE, name = "n_sp") %>%
    mutate(TraitName = factor(TraitName) %>%
             forcats::fct_reorder(n_sp))
}
