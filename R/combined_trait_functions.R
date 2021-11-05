consolidate_trait_names = function(bien_try_convert_df, try_traits) {
  bien_try_convert_df %>%
    rename(TraitID = trait_ids) %>%
    tidyr::unnest(TraitID) %>%
    full_join(try_traits %>%
                select(TraitID, Trait),
              by = "TraitID") %>%
    mutate(consolidated_trait_name = case_when(
      !is.na(trait_name) ~ trait_name,
      TRUE ~ Trait
    )) %>%
    rename(bien_trait_name = trait_name,
           try_trait_id    = TraitID,
           try_trait_name  = Trait)
}

combine_bien_try_traits = function(consolidated_trait_names, glonaf_bien_traits,
                                   glonaf_try_traits_available) {
  bien_distinct_traits = glonaf_bien_traits %>%
    distinct(scrubbed_species_binomial, trait_name)

  try_distinct_traits = glonaf_try_traits_available %>%
    distinct(species_accepted_try, TraitName)

  list(
    consolidated_trait_names %>%
      inner_join(bien_distinct_traits,
                 by = c(bien_trait_name = "trait_name")) %>%
      rename(species = scrubbed_species_binomial),
    consolidated_trait_names %>%
      inner_join(try_distinct_traits,
                 by = c(try_trait_name = "TraitName")) %>%
      rename(species = species_accepted_try)
  ) %>%
    bind_rows() %>%
    distinct(consolidated_trait_name, species)
}

rank_species_trait_number = function(
  glonaf_bien_traits_count, try_total_number_trait,
  glonaf_try_traits_available, harmonized_try_glonaf) {

  list(
    bien = glonaf_bien_traits_count %>%
      select(-count) %>%
      group_by(species = scrubbed_species_binomial) %>%
      summarise(trait_number = n()),

    try_full = try_total_number_trait %>%
      select(species = species_accepted_try, trait_number),

    try_extract = glonaf_try_traits_available %>%
      filter(!is.na(TraitID)) %>%
      distinct(species = species_accepted_try, TraitID) %>%
      group_by(species) %>%
      summarise(trait_number = n())
  ) %>%
    bind_rows(.id = "trait_db") %>%
    group_by(trait_db) %>%
    full_join(
      harmonized_try_glonaf %>%
        distinct(species = species_accepted_try) %>%
        pull(species) %>%
        expand.grid(trait_db = c("bien", "try_full", "try_extract")) %>%
        rename(species = Var1),
      by = c("trait_db", "species")
    ) %>%
    mutate(trait_number = ifelse(is.na(trait_number), 0, trait_number)) %>%
    arrange(desc(trait_number)) %>%
    mutate(species_trait_number_rank = row_number()) %>%
    ungroup()
}

count_tuples_of_traits = function(list_of_lists, number_of_traits, trait_db) {
  list_of_lists %>%
    purrr::keep(~ length(.x) > (number_of_traits - 1)) %>%
    purrr::map(~ combn(.x, number_of_traits, simplify = FALSE)) %>%
    purrr::modify_depth(2, sort) %>%
    purrr::flatten() %>%
    purrr::map_chr(paste, collapse = " x ") %>%
    table() %>%
    sort(decreasing = TRUE) %>%
    tibble::enframe(name = "trait_combination", value = "n_species") %>%
    mutate(trait_number = number_of_traits,
           trait_db     = trait_db)
}

generate_all_possible_diaz_combinations = function(try_traits) {

  # List all possible combinations of traits that could be considered
  # valid list of Diaz traits
  try_diaz_comb = purrr::cross(
    list(seed_mass = 26,
         plant_height = 3106,
         leaf_area = c(3108:3113),
         leaf_mass_area = c(3086, 3115:3117),
         leaf_nitrogen_mass = 14,
         wood_density = 4)
  ) %>%
    purrr::map(
      ~.x %>%
        tibble::enframe("trait_code", "TraitID") %>%
        mutate(TraitID = as.numeric(TraitID)) %>%
        select(-trait_code) %>%
        inner_join(try_traits %>%
                     select(Trait, TraitID),
                   by = "TraitID") %>%
        pull(Trait)
    )

}

generate_all_possible_lhs_combinations = function(try_traits) {

  # List all possible combinations of traits that could be considered
  # valid list of LHS traits
  try_lhs_comb = purrr::cross(
    list(seed_mass = 26,
         plant_height = 3106,
         leaf_mass_area = c(3086, 3115:3117))
  ) %>%
    purrr::map(
      ~.x %>%
        tibble::enframe("trait_code", "TraitID") %>%
        mutate(TraitID = as.numeric(TraitID)) %>%
        select(-trait_code) %>%
        inner_join(try_traits %>%
                     select(Trait, TraitID),
                   by = "TraitID") %>%
        pull(Trait)
    )

}

count_lhs_diaz_combination_trait = function(trait_comb, try_diaz_combs,
                                            try_lhs_combs, db) {

  # Define allowable LHS combinations
  if(db == "bien") {
    lhs = list(
      c("leaf area per leaf dry mass", "seed mass", "whole plant height")
    )
  } else if (db == "austraits") {
    lhs = list(
      c("specific_leaf_area", "seed_mass", "plant_height")
    )
  } else {
    lhs = try_lhs_combs
  }

  # Define allowable Diaz traits combinations
  if(db == "bien") {
    diaz = list(
      c(
        "leaf area per leaf dry mass", "seed mass", "whole plant height",
        "leaf area", "stem wood density",
        "leaf nitrogen content per leaf dry mass"
      )
    )
  } else if (db == "austraits") {
    diaz = list(
      c("specific_leaf_area", "seed_mass", "plant_height", "leaf_area",
        "wood_density", "leaf_N_per_dry_mass")
    )
  } else {
    diaz = try_diaz_combs
  }

  trait_comb %>%
    rowwise() %>%
    mutate(
      contains_lhs = lhs %>%  # For each LHS combination
        purrr::map_lgl(~ all(.x %in% trait_names)) %>%  # Test if measured
        any(),  # Any of them is fine
      contains_diaz = diaz %>%  # For each Diaz combination
        purrr::map_lgl(~ all(.x %in% trait_names)) %>%  # Test if measured
        any()  # Any of them is fine
    )
}
