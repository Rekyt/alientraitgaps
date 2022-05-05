consolidate_trait_names = function(bien_try_convert_df, aus_try_convert_df,
                                   aus_bien_convert_df, gift_try_convert_df,
                                   try_traits) {

  # Get BIEN <-> TRY traits tabke
  bien_try_convert_df %>%
    rename(
      bien_trait_name = trait_name,
      try_trait_id    = trait_ids) %>%
    tidyr::unnest(try_trait_id) %>%
    # Add all TRY traits (with corresponding full names)
    full_join(
      try_traits %>%
        select(TraitID, try_trait_name = Trait),
      by = c(try_trait_id = "TraitID")
    ) %>%
    # Add AusTrait with correspondence to TRY traits
    full_join(
      aus_try_convert_df %>%
        tidyr::unnest(try_trait_id) %>%
        filter(!is.na(try_trait_id)),
      by = c("try_trait_name", "try_trait_id")
    ) %>%
    # Add GIFT traits corresponding to TRY
    full_join(
      gift_try_convert_df %>%
        tidyr::unnest(try_trait_id = trait_ids) %>%
        filter(!is.na(try_trait_id)) %>%
        rename(gift_trait_name = Trait2),
      by = "try_trait_id"
    ) %>%
    # Add AusTraits with no correspondence in TRY but in BIEN
    bind_rows(
      aus_bien_convert_df
    ) %>%
    # Add the rest of AusTraits (not in TRY nor in BIEN)
    bind_rows(
      aus_try_convert_df %>%
        tidyr::unnest(try_trait_id) %>%
        filter(is.na(try_trait_id)) %>%
        anti_join(aus_bien_convert_df, by = "aus_trait_name")
    ) %>%
    # Consolidate all names
    # Naming is BIEN > AusTraits > GIFT > TRY
    mutate(consolidated_name = case_when(
      !is.na(bien_trait_name) ~ bien_trait_name,
      !is.na(aus_trait_name)  ~ aus_trait_name,
      !is.na(gift_trait_name) ~ gift_trait_name,
      TRUE ~ try_trait_name
    )) %>%
    select(
      bien_trait_name, aus_trait_name, try_trait_id, try_trait_name,
      gift_trait_name, consolidated_name
    )
}

combine_bien_try_aus_gift_traits = function(
    consolidated_trait_names, glonaf_bien_traits, glonaf_try_traits_available,
    aus_traits, gift_glonaf_traits
) {
  bien_distinct_traits = glonaf_bien_traits %>%
    distinct(species = scrubbed_species_binomial, bien_trait_name = trait_name)

  aus_distinct_traits = aus_traits %>%
    distinct(species = species_accepted_austraits, aus_trait_name = trait_name)

  try_distinct_traits = glonaf_try_traits_available %>%
    distinct(species = species_accepted_try, try_trait_id = TraitID)

  gift_distinct_traits = gift_glonaf_traits %>%
    distinct(species = species_accepted_gift, gift_trait_name = Trait2)

  list(
    # BIEN
    consolidated_trait_names %>%
      inner_join(bien_distinct_traits,
                 by = "bien_trait_name"),
    # AusTraits
    consolidated_trait_names %>%
      inner_join(aus_distinct_traits,
                 by = "aus_trait_name"),
    # TRY
    consolidated_trait_names %>%
      inner_join(try_distinct_traits,
                 by = "try_trait_id"),
    # GIFT
    consolidated_trait_names %>%
    inner_join(gift_distinct_traits,
               by = "gift_trait_name")
  ) %>%
    bind_rows() %>%
    distinct(consolidated_name, species)
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

get_bergmann_combs = function() {
  # 4 traits Bergmann et al. 2020
  # For the analysis we used four root traits:
  # root average diameter (RAD; mm),
  root_diameter = c(
    "Fine root diameter" , "thickest_root_diameter",
    "Fine root (absorptive) diameter", "Fine root (transport) diameter",
    "Coarse root diameter"
  )
  # root tissue density (RTD; root DW per volume mg cm−3),
  root_tissue_density = c(
    "Fine root tissue density (fine root dry mass per fine root volume)",
    "Fine root (absorptive) tissue density (absorptive fine root dry mass per absorptive fine root volume",
    "Fine root (transport) tissue density (transport fine root dry mass per transport fine root volume)",
    "Root dry mass per root volume (root density, root tissue density)",
    "Coarse root tissue density (coarse root dry mass per coarse root volume)"
  )
  # specific root length (SRL; cm mg−1),
  specific_root_length = c(
    "Fine root length per fine root dry mass (specific fine root length, SRL)",
    "specific_root_length",
    "Fine root (absorptive) length per absorptive fine root dry mass (specific absorptive fine root lengt"
  )
  # Root nitrogen
  root_nitrogen = c(
    "Fine root nitrogen (N) content per fine root dry mass",
    "Fine root (absorptive) nitrogen (N) content per absorptive fine root dry mass",
    "Fine root (transport) nitrogen (N) content per transport fine root dry mass"
  )

  bergmann_combs = purrr::cross(
    list(
      root_diameter = root_diameter,
      root_tissue_density = root_tissue_density,
      specific_root_length = specific_root_length,
      root_nitrogen = root_nitrogen
    )
  ) %>%
    purrr::map(as.character)
}

count_specific_trait_combinations = function(combined_traits, match_glonaf_tnrs,
                                             bergmann_comb_df) {
  combined_traits %>%
    group_by(species) %>%
    summarise(traits = list(consolidated_name)) %>%
    right_join(match_glonaf_tnrs %>%
                 select(Name_matched),
               by = c(species = "Name_matched")) %>%
    rowwise() %>%
    mutate(
      in_glonaf              = TRUE,
      has_at_least_one_trait = length(traits) > 0,
      has_lhs                = all(
        c("leaf area per leaf dry mass", "seed mass", "whole plant height") %in%
          traits
      ),
      has_diaz               = all(
        c(
          "leaf area per leaf dry mass", "seed mass", "whole plant height",
          "leaf area", "stem wood density",
          "leaf nitrogen content per leaf dry mass"
        ) %in% traits
      ),
      has_bergmann = bergmann_comb_df %>%
        purrr::map_lgl(~ all(.x %in% traits)) %>%
        any()
    )
}

combine_trait_categories = function(
  consolidated_trait_names, gift_trait_categories, aus_trait_categories,
  bien_trait_categories, try_trait_categories
) {
  all_trait_categories = consolidated_trait_names %>%
    full_join(
      gift_trait_categories %>%
        rename(gift_trait_name = Trait2, gift_trait_cat = trait_cat),
      by = "gift_trait_name"
    ) %>%
    full_join(
      aus_trait_categories %>%
        rename(aus_trait_cat = trait_cat),
      by = "aus_trait_name"
    ) %>%
    full_join(
      bien_trait_categories %>%
        rename(bien_trait_name = trait_name, bien_trait_cat = trait_cat),
      by = "bien_trait_name"
    ) %>%
    full_join(
      try_trait_categories %>%
        rename(try_trait_cat = trait_cat),
      by = "consolidated_name"
    ) %>%
    filter(!is.na(consolidated_name))

  all_trait_categories %>%
    select(consolidated_name, ends_with("trait_cat")) %>%
    tidyr::nest(trait_cat_df = !consolidated_name) %>%
    mutate(trait_cat_sum = purrr::map(trait_cat_df, distinct),
           final_trait_cat = purrr::map(
             trait_cat_sum,
             ~.x %>%
               unlist() %>%
               unique() %>%
               na.exclude() %>%
               as.character()
           ) %>%
             purrr::modify_if(~ length(.) == 0, ~ NA_character_) %>%
             as.character()
    ) %>%
    select(consolidated_name, trait_category = final_trait_cat)
}

count_trait_categories_per_species = function(
  combined_trait_categories_species, match_glonaf_tnrs
) {
  combined_trait_categories_species %>%
    # Count number of traits for each category per sepcies
    count(species, trait_category) %>%
    # Remove NA categories
    filter(!is.na(trait_category)) %>%
    # Transform into a table where each category is a separate column
    tidyr::pivot_wider(
      names_from = trait_category, values_from = n, values_fill = 0
    ) %>%
    # Add back species with no trait values
    full_join(match_glonaf_tnrs %>%
                distinct(species = Accepted_name),
              by = "species") %>%
    mutate(across(where(is.numeric), ~ifelse(is.na(.x), 0, .x)))
}
