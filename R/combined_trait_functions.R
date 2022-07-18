read_correspondence_tables = function(raw_correspondence_tables) {

  names(raw_correspondence_tables) = gsub(
    "_correspondence.ods", "", basename(raw_correspondence_tables), fixed = TRUE
  )

  lapply(raw_correspondence_tables, function(x) {
    readODS::read_ods(x) %>%
      as_tibble()
  })
}

check_correspondence_tables = function(
  correspondence_tables, austraits, gift_traits_meta, try_traits
) {

  corres_df = correspondence_tables

  # Define Trait Names
  aus_names = names(austraits$definitions$traits$elements)

  bien_names = BIEN::BIEN_trait_list()[["trait_name"]] %>%
    na.omit() %>%
    as.character()

  gift_names = gift_traits_meta %>%
    distinct(Trait2, Lvl3)


  # Check values in columns 'identical' and 'similar'
  ident_similar = corres_df %>%
    purrr::imap_dfr(
      ~.x %>%
                      distinct(identical, similar) %>%
                      mutate(table = .y) %>%
                      select(table, everything()) %>%
                      arrange(table, identical, similar)
  ) %>%
    distinct(identical, similar)


  if (ident_similar[["identical"]] != c("no", "yes", NA) ||
      ident_similar[["similar"]] != c("yes", "yes", NA)) {
    stop("Issue with identical and similar columns")
  }


  # Check that trait names are indeed in database
  # Check AusTraits names
  aus_1 = corres_df$austraits_bien %>%
    filter(!(austraits_trait_name %in% aus_names))

  aus_2 = corres_df$austraits_try %>%
    filter(!(austraits_trait_name %in% aus_names))

  aus_3 = corres_df$gift_austraits %>%
    filter(
      !(austraits_trait_name %in% aus_names) & !is.na(austraits_trait_name)
  )

  # Check BIEN names
  bien_1 = corres_df$austraits_bien %>%
    filter(!(bien_trait_name %in% bien_names) & !is.na(bien_trait_name))

  bien_2 = corres_df$bien_gift %>%
    filter(!(bien_trait_name %in% bien_names) & !is.na(bien_trait_name))

  bien_3 = corres_df$bien_try %>%
    filter(!(bien_trait_name) %in% bien_names)

  # Check GIFT names
  gift_1 = corres_df$bien_gift %>%
    filter(
      (!(gift_trait_id %in% gift_names$Lvl3) |
         !(gift_trait_name %in% gift_names$Trait2)) & (!is.na(gift_trait_id) &
                                                         !is.na(gift_trait_name))
    )

  gift_2 = corres_df$gift_austraits %>%
    filter(!(gift_trait_id %in% gift_names$Lvl3) |
             !(gift_trait_name %in% gift_names$Trait2))

  gift_3 = corres_df$gift_try %>%
    filter(!(gift_trait_name %in% gift_names$Trait2))

  # Check TRY names
  try_1 = corres_df$austraits_try %>%
    filter(
      (!(try_trait_id %in% try_traits$TraitID)) &
        (!is.na(try_trait_id) & !is.na(try_trait_name))
    )

  try_2 = corres_df$bien_try %>%
    filter(
      (!(try_trait_id %in% try_traits$TraitID) |
         !(try_trait_name %in% try_traits$Trait)) &
        (!is.na(try_trait_id) & !is.na(try_trait_name))
    )

  try_3 = corres_df$gift_try %>%
    filter(
      (!(try_trait_id %in% try_traits$TraitID) |
         !(try_trait_name %in% try_traits$Trait)) &
        (!is.na(try_trait_id) & !is.na(try_trait_name))
    )

  rows = list(
    aus_1, aus_2, aus_3, bien_1, bien_2, bien_3, gift_1, gift_2, gift_3, try_1,
    try_2, try_3
  ) %>%
    vapply(nrow, 1L)

  if (any(rows != 0)) {
    stop("Some rows were not empty")
  }

  return(corres_df)
}


create_trait_network = function(
  correspondence_tables_check, austraits, gift_traits_meta, try_traits
) {

  corres_df = correspondence_tables_check

  # Get back trait names
  aus_names = names(austraits$definitions$traits$elements)

  bien_names = BIEN::BIEN_trait_list()[["trait_name"]] %>%
    na.omit() %>%
    as.character()

  gift_names = gift_traits_meta %>%
    distinct(Trait2, Lvl3)


  # Create nodes data.frame
  node_df = data.frame(
    name = c(aus_names, bien_names, gift_names$Trait2, try_traits$TraitID),
    database = c(
      rep("AusTraits", length.out = length(aus_names)),
      rep("BIEN",      length.out = length(bien_names)),
      rep("GIFT",      length.out = nrow(gift_names)),
      rep("TRY",       length.out = nrow(try_traits))
    )
  )

  # Unified edge data.frame
  corres_df$gift_austraits = corres_df$gift_austraits %>%
    select(-gift_trait_id)

  corres_df$bien_gift = corres_df$bien_gift %>%
    select(-gift_trait_id)

  edge_df = corres_df %>%
    purrr::map_dfr(
      function(x) {
        smaller_df = x %>%
          select(1:2, identical, similar) %>%
          mutate(across(.fns = as.character))

        colnames(smaller_df)[1:2] = c("from", "to")

        smaller_df %>%
          filter(!is.na(to))
      })


  # Create trait network
  trait_network = tidygraph::tbl_graph(
    nodes = node_df %>%
      mutate(name = gsub(" ", "__", name, fixed = TRUE)),
    edges = edge_df %>%
      mutate(from = gsub(" ", "__", from, fixed = TRUE),
             to   = gsub(" ", "__",   to, fixed = TRUE))
  )
}

consolidate_trait_names_from_network = function(trait_network, try_traits) {

  # Extract all connected components seperately
  all_components = trait_network %>%
    tidygraph::to_components()

  component_size = all_components %>%
    purrr::map_dbl(
      ~.x %>%
        tidygraph::activate(nodes) %>%
        length()
    )

  # Look at some of the biggest connected component
  all_components[which(component_size > 10)]


  db_df = data.frame(
    database = c("AusTraits", "BIEN", "GIFT", "TRY"),
    trait_name = c(
      "austraits_trait_name", "bien_trait_name", "gift_trait_name",
      "try_trait_id"
    )
  )

  # Get unified trait table
  all_traits = purrr::map_dfr(all_components, function(x) {
    node_df = x %>%
      tidygraph::activate(nodes) %>%
      as.data.frame()

    has_only_try = length(node_df[["database"]]) == 1 &
      ("TRY" %in% node_df[["database"]])

    node_df = node_df %>%
      full_join(db_df, by = "database") %>%
      arrange(trait_name)

    node_df$name = gsub("__", " ", node_df$name, fixed = TRUE)

    # Add Consolidated Name
    if (!has_only_try) {

      first_non_na_trait = node_df %>%
        filter(!is.na(name)) %>%
        pull(name) %>%
        .[1]

      node_df = node_df %>%
        add_row(
          name = first_non_na_trait,
          trait_name = "consolidated_name"
        )

    } else {


      first_try_trait = node_df %>%
        filter(database == "TRY") %>%
        slice(1)

      try_name = try_traits %>%
        filter(TraitID == first_try_trait[["name"]]) %>%
        pull(Trait) %>%
        .[1]

      node_df = node_df %>%
        add_row(
          name = try_name,
          trait_name = "consolidated_name"
        )
    }

    node_df %>%
      select(-database) %>%
      tidyr::pivot_wider(
        names_from = trait_name, values_from = name, values_fn = list
      )
  }) %>%
    tidyr::unnest(austraits_trait_name) %>%
    tidyr::unnest(bien_trait_name) %>%
    tidyr::unnest(gift_trait_name) %>%
    tidyr::unnest(try_trait_id) %>%
    tidyr::unnest(consolidated_name) %>%
    left_join(
      try_traits %>%
        distinct(try_trait_id = TraitID, try_trait_name = Trait) %>%
        mutate(try_trait_id = as.character(try_trait_id)),
      by = "try_trait_id"
    )

  # Rename and re-order column as the initial consolidated trait name df
  all_traits %>%
    rename(aus_trait_name = austraits_trait_name) %>%
    select(
      bien_trait_name, aus_trait_name, try_trait_id, try_trait_name,
      gift_trait_name, consolidated_name
    )
}


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
      inner_join(bien_distinct_traits, by = "bien_trait_name"),
    # AusTraits
    consolidated_trait_names %>%
      inner_join(aus_distinct_traits, by = "aus_trait_name"),
    # TRY
    consolidated_trait_names %>%
      inner_join(
        try_distinct_traits %>%
          mutate(try_trait_id = as.character(try_trait_id)),
        by = "try_trait_id"
      ),
    # GIFT
    consolidated_trait_names %>%
      inner_join(gift_distinct_traits, by = "gift_trait_name")
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

# Function to create a unified growth form dataset to use downstream
extract_growth_form = function(
  combined_traits, glonaf_bien_traits, gift_traits_final, gift_names_traits,
  harmonized_gift_glonaf, match_glonaf_tnrs
) {
  # Extract growth form data from BIEN
  bien_growth_form = glonaf_bien_traits %>%
    filter(grepl("whole plant growth form",trait_name)) %>%
    distinct(
      species = scrubbed_species_binomial, trait_name, growth_form = trait_value
    )

  # Extract growth form data from GIFT
  gift_growth_form = gift_traits_final %>%
    filter(trait_ID == "1.2.1") %>%
    distinct(work_ID, trait_value) %>%
    # Add back species names
    inner_join(
      gift_names_traits %>%
        distinct(work_ID, species),
      by = "work_ID"
    ) %>%
    # Re-add harmonized species names
    inner_join(
      harmonized_gift_glonaf %>%
        distinct(species = name_init_gift, species_accepted_gift),
      by = "species"
    ) %>%
    # Keep only harmonized species names and growth form value
    distinct(species = species_accepted_gift, trait_value) %>%
    select(species, growth_form = trait_value)

  # Species without growth form in GIFT
  species_missing_growth_form = combined_traits %>%
    distinct(species) %>%
    anti_join(
      gift_growth_form %>%
        filter(!is.na(growth_form)),
      by = "species"
    )

  # Unify growth forms in BIEN for missing species of GIFT
  miss_species_growth_bien = bien_growth_form %>%
    semi_join(
      species_missing_growth_form, by = "species"
    ) %>%
    filter(trait_name == "whole plant growth form") %>%
    mutate(
      growth_form = growth_form %>%
        stringr::str_to_title() %>%
        stringr::str_replace(stringr::fixed("*"), "")
    ) %>%
    mutate(
      growth_form =
        case_when(
          growth_form %in% c("Shrub", "Tree", "Herb") ~ growth_form,
          growth_form %in% c("Forb", "Grass", "Fern") ~ "Herb",
          growth_form == "Small_tree"                 ~ "Tree",
          TRUE                                        ~ "Other"
        ) %>%
        tolower()
    ) %>%
    distinct(species, growth_form)

  combined_traits %>%
    distinct(species) %>%
    # Add GloNAF species that show NO trait data
    full_join(
      match_glonaf_tnrs %>%
        distinct(species = Accepted_name),
      by = "species"
    ) %>%
    # Add growth trait datasets
    left_join(
      list(
        gift_growth_form %>%
          filter(!is.na(growth_form)),
        miss_species_growth_bien
      ) %>%
        bind_rows(),
      by = "species"
    ) %>%
    # Any species without growth form should be labelled "unknown"
    mutate(growth_form = ifelse(is.na(growth_form), "unknown", growth_form))
}
