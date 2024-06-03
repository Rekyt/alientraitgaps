read_correspondence_tables = function(raw_correspondence_tables) {

  names(raw_correspondence_tables) = gsub(
    "_correspondence_v2.ods", "", basename(raw_correspondence_tables), fixed = TRUE
  )

  lapply(raw_correspondence_tables, function(x) {
    readODS::read_ods(x, na = c("", "NA")) %>%
      as_tibble()
  })
}

check_correspondence_tables = function(
    correspondence_tables, bien_trait_list, gift_trait_meta, try_traits,
    apd_bien, apd_gift, apd_try
) {

  corres_df = correspondence_tables

  bien_names = bien_trait_list[["trait_name"]] %>%
    na.omit() %>%
    as.character()

  gift_names = gift_trait_meta %>%
    distinct(Trait1, Lvl2, Trait2, Lvl3)


  # Check values in columns 'identical' and 'similar'
  ident_similar = corres_df %>%
    purrr::imap_dfr(
      ~.x %>%
        distinct(identical, similar, match) %>%
        mutate(table = .y) %>%
        select(table, everything()) %>%
        arrange(table, identical, similar, match)
    ) %>%
    distinct(identical, similar, match) |>
    arrange(identical)


  if (
    !identical(ident_similar[["identical"]], c("no", "no", "yes", "yes", NA)) |
    !identical(ident_similar[["similar"]], c(rep("yes", 4), NA)) |
    !identical(
      ident_similar[["match"]], c("close", "related", "exact", "close", NA)
    )
  ) {
    stop("Issue with identical, similar, and match columns")
  }

  # Check that trait names in correspondence tables are referenced
  # BIEN
  names_bien_austraits = apd_bien |>
    filter(!(extracted_trait %in% bien_names))

  names_bien_gift = corres_df$bien_gift %>%
    filter(!(bien_trait_name %in% bien_names) & !is.na(bien_trait_name))

  names_bien_try = corres_df$bien_try %>%
    filter(!(bien_trait_name) %in% bien_names)

  # Check GIFT names
  names_gift_austraits = apd_gift |>
    filter(
      !(extracted_trait %in% c(gift_trait_meta$Lvl2, gift_trait_meta$Lvl3))
    )

  names_gift_bien = corres_df$bien_gift %>%
    filter(
      (!(gift_trait_id %in% gift_names$Lvl3) |
         !(gift_trait_name %in% gift_names$Trait2)) &
        (!is.na(gift_trait_id) & !is.na(gift_trait_name))
    )

  names_gift_try = corres_df$gift_try %>%
    filter(!(gift_trait_name %in% gift_names$Trait2))

  # Check TRY names
  names_try_austraits = apd_try %>%
    filter(!(extracted_trait %in% try_traits$TraitID))

  names_try_bien = corres_df$bien_try %>%
    filter(
      (!(try_trait_id %in% try_traits$TraitID) |
         !(try_trait_name %in% trimws(try_traits$Trait))) &
        (!is.na(try_trait_id) & !is.na(try_trait_name))
    )

  names_try_gift = corres_df$gift_try %>%
    filter(
      (!(try_trait_id %in% try_traits$TraitID) |
         !(try_trait_name %in% try_traits$Trait)) &
        (!is.na(try_trait_id) & !is.na(try_trait_name))
    )

  correspondence_list = list(
    names_bien_austraits, names_bien_gift, names_bien_try, names_gift_austraits,
    names_gift_bien, names_gift_try, names_try_austraits, names_try_bien,
    names_try_gift
  )

  rows = correspondence_list |>
    vapply(nrow, 1L)

  if (any(rows != 0)) {
    stop("Some rows were not empty")
  }

  return(corres_df)
}


create_trait_network = function(
    correspondence_tables_check, bien_trait_list, gift_trait_meta, try_traits,
    apd_subset, apd_bien, apd_gift, apd_try
) {

  corres_df = correspondence_tables_check

  # Get back trait names
  aus_names = apd_subset[["trait"]]

  bien_names = bien_trait_list[["trait_name"]] %>%
    na.omit() %>%
    as.character()

  gift_names = gift_trait_meta %>%
    distinct(Trait1, Lvl2, Trait2, Lvl3)

  # Manage the issue of APD-GIFT matching Lvl2 and Lvl3 traits
  apd_gift_lvl3 = apd_gift |>
    filter(gift_lvl == "Lvl3")

  apd_gift_lvl2 = apd_gift |>
    filter(gift_lvl == "Lvl2") |>
    inner_join(
      gift_names |>
        select(Lvl2, Trait2, Lvl3),
      by = c(extracted_trait = "Lvl2"),
      relationship = "many-to-many"
    ) |>
    select(-extracted_trait, -Trait2) |>
    rename(extracted_trait = Lvl3)

  # Transform GIFT APD Lvl2 traits into their Lvl3 equivalent
  apd_gift_updated = bind_rows(apd_gift_lvl3, apd_gift_lvl2) |>
    inner_join(
      gift_names |>
        distinct(Trait2, Lvl3),
      by = c(extracted_trait = "Lvl3")
    ) |>
    select(-extracted_trait, extracted_trait = Trait2)


  # Create nodes data.frame
  node_df = data.frame(
    name = c(aus_names, bien_names, gift_names$Trait2, try_traits$TraitID),
    database = c(
      rep("AusTraits", length.out = length(aus_names)),
      rep("BIEN",      length.out = length(bien_names)),
      rep("GIFT",      length.out = nrow(gift_names)),
      rep("TRY",       length.out = nrow(try_traits))
    ),
    alternative_name = c(
      apd_subset[["label"]], bien_names, gift_names[["Trait2"]],
      try_traits[["Trait"]]
    )
  )

  # Unified edge data.frame
  corres_df$bien_gift = corres_df$bien_gift %>%
    select(-gift_trait_id)

  edge_df = corres_df %>%
    purrr::map_dfr(
      function(x) {
        smaller_df = x %>%
          select(1:2, match_type = match) %>%
          mutate(across(everything(), .fns = as.character))

        colnames(smaller_df)[1:2] = c("from", "to")

        smaller_df %>%
          filter(!is.na(to))
      })

  apd_edge_df = list(
    apd_bien, apd_gift_updated, apd_try
  ) |>
    purrr::map_dfr(
      function(x) {
        smaller_df = x %>%
          select(from = trait, to = extracted_trait, match_type) %>%
          mutate(across(everything(), .fns = as.character))

        smaller_df %>%
          filter(!is.na(to))
      })

  edge_df = bind_rows(edge_df, apd_edge_df)

  # Create trait network
  tidygraph::tbl_graph(
    nodes = node_df %>%
      mutate(name = gsub(" ", "__", name, fixed = TRUE)),
    edges = edge_df %>%
      mutate(from = gsub(" ", "__", from, fixed = TRUE),
             to   = gsub(" ", "__",   to, fixed = TRUE))
  )
}

write_network_file = function(trait_network, filepath) {

  igraph::write_graph(trait_network, filepath, "graphml")

  filepath
}


name_connected_components = function(trait_network) {

  trait_network |>
    tidygraph::activate(nodes) |>
    mutate(component = tidygraph::group_components()) |>
    arrange(component, database) |>
    group_by(component) |>
    mutate(component_name = alternative_name[[1]],
           component_size = n()) |>
    ungroup() |>
    as_tibble()

}


consolidate_trait_names_from_network = function(trait_network, match_type = c("full", "close", "exact")) {

  if (match_type == "close") {

    trait_network = trait_network |>
      tidygraph::activate(edges) |>
      filter(match_type != "related")

  } else if (match_type == "exact") {

    trait_network = trait_network |>
      tidygraph::activate(edges) |>
      filter(match_type == "exact")

  }

  db_df = data.frame(
    database = c("AusTraits", "BIEN", "GIFT", "TRY"),
    trait_name = c(
      "austraits_trait_name", "bien_trait_name", "gift_trait_name",
      "try_trait_id"
    )
  )

  nodes_df = name_connected_components(trait_network) |>
    left_join(db_df, by = "database") |>
    select(-database, -alternative_name) |>
    rename(consolidated_name = component_name) |>
    tidyr::pivot_wider(
      names_from = trait_name, values_from = name, values_fn = list, values_fill = list(NA_character_)
    )

}

unnest_names = function(trait_names_df) {

  trait_names_df |>
    tidyr::unnest(austraits_trait_name) |>
    tidyr::unnest(bien_trait_name) |>
    tidyr::unnest(gift_trait_name) |>
    tidyr::unnest(try_trait_id) |>
    mutate(
      bien_trait_name = gsub("__", " ", bien_trait_name, fixed = TRUE)
    )

}


combine_traits_all_databases = function(
    trait_names, austraits_traits_simple, bien_traits_simple,
    gift_traits_simple, try_traits_simple, gift_trait_meta, austraits_glonaf,
    bien_glonaf, gift_glonaf, try_glonaf
) {

  ## AusTraits
  austraits_consolidated = trait_names |>
    # Add species names to trait table
    inner_join(
      austraits_traits_simple, by = c(austraits_trait_name = "trait_name"),
      relationship = "many-to-many"
    ) |>
    distinct(component, consolidated_name, component_size, taxon_name) |>
    # Keep only matched GloNAF species
    inner_join(austraits_glonaf, by = "taxon_name") |>
    distinct(
      component, consolidated_name, component_size, matched_name = binomial
    )

  ## BIEN
  bien_consolidated = trait_names |>
    # Add species names to trait table
    inner_join(
      bien_traits_simple, by = c(bien_trait_name = "trait_name"),
      relationship = "many-to-many"
    ) |>
    distinct(
      component, consolidated_name, component_size, scrubbed_species_binomial
    ) |>
    semi_join(bien_glonaf, by = "scrubbed_species_binomial") |>
    distinct(
      component, consolidated_name, component_size,
      matched_name = scrubbed_species_binomial
    )

  ## GIFT
  gift_consolidated = gift_traits_simple |>
    inner_join(
      gift_trait_meta |>
        select(trait_ID = Lvl3, trait_name = Trait2),
      by = "trait_ID"
    ) |>
    inner_join(
      trait_names, by = c(trait_name = "gift_trait_name"),
      relationship = "many-to-many"
    ) |>
    distinct(component, consolidated_name, component_size, work_species) |>
    # Keep only matched GloNAF species
    semi_join(gift_glonaf, by = "work_species") |>
    rename(matched_name = work_species)

  ## TRY
  try_consolidated = trait_names |>
    mutate(try_trait_id = as.integer(try_trait_id)) |>
    inner_join(
      try_traits_simple, by = c(try_trait_id = "TraitID"),
      relationship = "many-to-many"
    ) |>
    distinct(component, consolidated_name, component_size, AccSpeciesID) |>
    # Keep only matched GloNAF species
    inner_join(try_glonaf, by = "AccSpeciesID") |>
    distinct(
      component, consolidated_name, component_size, matched_name = MatchedName
    )


  full_consolidated = list(
    austraits = austraits_consolidated,
    bien = bien_consolidated,
    gift = gift_consolidated,
    try = try_consolidated
  ) |>
    bind_rows(.id = "database") |>
    rename(species = matched_name)

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

count_traits_per_database = function(
    network_consolidated_trait_names, glonaf_bien_traits,
    glonaf_try_traits_available, aus_traits, gift_glonaf_traits
) {

  bien_traits = glonaf_bien_traits %>%
    select(
      species = scrubbed_species_binomial, bien_trait_name = trait_name,
    ) %>%
    mutate(origin = "BIEN")

  aus_traits = aus_traits %>%
    select(
      species = species_accepted_austraits, aus_trait_name = trait_name,
    ) %>%
    mutate(origin = "AusTraits")

  try_traits = glonaf_try_traits_available %>%
    select(
      species = species_accepted_try, try_trait_id = TraitID
    ) %>%
    mutate(origin = "TRY")

  gift_traits = gift_glonaf_traits %>%
    select(
      species = species_accepted_gift, gift_trait_name = Trait2
    ) %>%
    mutate(origin = "GIFT")

  list(
    # BIEN
    network_consolidated_trait_names %>%
      inner_join(bien_traits, by = "bien_trait_name"),
    # AusTraits
    network_consolidated_trait_names %>%
      inner_join(aus_traits, by = "aus_trait_name"),
    # TRY
    network_consolidated_trait_names %>%
      inner_join(
        try_traits %>%
          mutate(try_trait_id = as.character(try_trait_id)),
        by = "try_trait_id"
      ),
    # GIFT
    network_consolidated_trait_names %>%
      inner_join(gift_traits, by = "gift_trait_name")
  ) %>%
    bind_rows() %>%
    select(consolidated_name, species, origin) %>%
    group_by(consolidated_name, origin, species) %>%
    summarise(n_measurements = n())
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

count_specific_trait_combinations = function(
    combined_traits, match_glonaf_tnrs, bergmann_comb_df
) {

  combined_traits %>%
    group_by(species) %>%
    summarise(traits = list(consolidated_name)) %>%
    right_join(
      match_glonaf_tnrs %>%
        distinct(species = Accepted_species) %>%
        filter(species != ""),
      by = "species"
    ) %>%
    rowwise() %>%
    mutate(
      in_glonaf              = TRUE,
      has_at_least_one_trait = length(traits) > 0,
      has_lhs                = all(
        c("specific_leaf_area", "diaspore_mass", "plant_height") %in%
          traits
      ),
      has_diaz               = all(
        c("specific_leaf_area", "diaspore_mass", "plant_height", "leaf_area",
          "wood_density", "leaf_N_per_dry_mass") %in% traits
      ),
      has_bergmann = bergmann_comb_df %>%
        purrr::map_lgl(~ all(.x %in% traits)) %>%
        any()
    )

}


# Function to create a unified growth form dataset to use downstream
extract_growth_form = function(
    combined_traits, glonaf_bien_traits, gift_all_raw_traits, gift_names_traits,
    harmonized_gift_glonaf, match_glonaf_tnrs, glonaf_list
) {
  # Reconstruct GloNAF df
  glonaf_names_df = data.frame(
    id           = paste0("glonaf-", seq_along(glonaf_list)),
    species_name = glonaf_list
  )

  # Extract growth form data from BIEN
  bien_growth_form = glonaf_bien_traits %>%
    filter(grepl("whole plant growth form",trait_name)) %>%
    distinct(
      species = scrubbed_species_binomial, trait_name, growth_form = trait_value
    )

  # Extract growth form data from GIFT
  gift_growth_form = gift_all_raw_traits %>%
    filter(trait_ID == "1.2.1") %>%
    distinct(work_ID, trait_value) %>%
    # Add back species names
    inner_join(
      gift_names_traits %>%
        distinct(work_ID, species = Accepted_species),
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
        inner_join(glonaf_names_df, by = c(ID = "id")) %>%
        distinct(species = Accepted_species),
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


simplify_growth_form = function(combined_growth_form) {

  combined_growth_form %>%
    # List all growth forms by species
    group_by(species) %>%
    summarise(all_forms = list(growth_form)) %>%
    # Simplify growth forms by using tree > shrub > herb > other > unknown
    rowwise() %>%
    mutate(
      simp_form = case_when(
        length(all_forms) == 1 ~ all_forms[[1]],
        "tree"  %in% all_forms ~ "tree",
        "shrub" %in% all_forms ~ "shrub",
        "herb"  %in% all_forms ~ "herb",
        "other" %in% all_forms ~ "other"
      )
    ) %>%
    ungroup() %>%
    select(-all_forms)

}

# Count Summary of Number of Traits per species per Region
count_number_of_traits_per_region = function(
    glonaf_species_regions, combined_traits
) {
  glonaf_species_regions %>%
    full_join(
      combined_traits %>%
        count(species, name = "n_traits"),
      by = "species"
    ) %>%
    mutate(n_traits = ifelse(is.na(n_traits), 0, n_traits)) %>%
    group_by(OBJIDsic) %>%
    summarise(
      across(n_traits, .fns = list(median = median, mean = mean, sd = sd))
    )
}

list_species_by_trait_per_database = function(combined_traits_origin) {

  combined_traits_origin %>%
    ungroup() %>%
    group_by(consolidated_name, database) %>%
    # List species per trait per database
    summarise(sp_list = list(species)) %>%
    # Get list of species common across database (with data)
    # and list of all species available
    mutate(common_sp = list(Reduce(intersect, sp_list)),
           all_sp    = list(Reduce(union, sp_list))) %>%
    # Count number of species across category
    rowwise() %>%
    mutate(
      n_species = length(sp_list),    # Number of species per database
      n_common  = length(common_sp),  # Number of species in common
      n_all     = length(all_sp)      # Total number of species
    ) %>%
    ungroup()

}

intersect_species_list_by_trait_across_database = function(
    trait_database_sp_list
) {

  trait_database_sp_list %>%
    # Sub-select only needed information
    select(consolidated_name, origin, sp_list, n_all) %>%
    # Get all combinations of trait name and database
    tidyr::complete(
      consolidated_name, origin, fill = list(sp_list = list(""))
    ) %>%
    # Fill NAs 'n_all' columns by non empty values
    group_by(consolidated_name) %>%
    arrange(desc(n_all), consolidated_name, origin) %>%
    mutate(n_all = zoo::na.locf(n_all)) %>%
    ungroup() %>%
    # Organize diagrams per number of species/trait name/database name
    arrange(desc(n_all), consolidated_name, origin) %>%
    group_by(consolidated_name) %>%
    # Create actual Euler diagrams
    summarise(
      euler = sp_list %>%
        setNames(origin) %>%
        eulerr::euler() %>%
        lst(),
      n_all = unique(n_all)
    ) %>%
    arrange(desc(n_all))

}
