remove_genus_only_names = function(species_list) {
  species_list[grepl(" ", species_list)]
}

remove_hybrid_names = function(species_list) {
  species_list[!grepl("^x .+", species_list, ignore.case = FALSE)]
}

remove_problematic_ending = function(species_list) {
  gsub(" var\\.$", "", species_list)
}

match_with_lcvp = function(species_list) {
  species_list %>%
    unique() %>%
    na.exclude() %>%  # Remove NAs
    as.character() %>%
    remove_genus_only_names() %>%
    remove_hybrid_names() %>%
    remove_problematic_ending() %>%
    lcvplants::lcvp_search()
}

harmonize_try_glonaf_species = function(match_try_tnrs, match_glonaf_tnrs) {
  # Subset most important columns
  sub_try = match_try_tnrs %>%
    distinct(
      name_init_try        = Search,
      status_try           = Status,
      species_accepted_try = Output.Taxon
    )

  sub_glonaf = match_glonaf_tnrs %>%
    distinct(
      name_init_glonaf        = Search,
      status_glonaf           = Status,
      species_accepted_glonaf = Output.Taxon
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


harmonize_try_open = function(full_try_df, harmonized_try_glonaf) {
  harmonized_try_glonaf %>%
    inner_join(full_try_df %>%
    disk.frame::chunk_distinct(AccSpeciesName) %>%
    collect() %>%
    distinct(AccSpeciesName),
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

list_trait_combination_per_species = function(glonaf_try_traits_available) {
  glonaf_try_traits_available %>%
    distinct(species_accepted_try, TraitID, TraitName) %>%
    tibble::as_tibble() %>%
    select(-TraitID) %>%
    group_by(species_accepted_try) %>%
    summarise(trait_names = list(TraitName))
}

list_bien_trait_combination_per_species = function(glonaf_bien_traits) {
  glonaf_bien_traits %>%
    filter(!is.na(trait_name)) %>%
    distinct(species = scrubbed_species_binomial, trait_name) %>%
    group_by(species) %>%
    summarise(trait_names = list(trait_name))
}
