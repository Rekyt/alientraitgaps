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


consolidate_trait_names_from_network = function(
    trait_network, match_type = c("full", "close", "exact")
) {

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
    trait_names, austraits_traits_harmo, bien_traits_simple, gift_traits_harmo,
    try_traits_harmo, glonaf_tnrs
) {

  ## AusTraits
  austraits_consolidated = trait_names |>
    # Add species names to trait table
    inner_join(
      austraits_traits_harmo, by = c(austraits_trait_name = "trait_name"),
      relationship = "many-to-many"
    ) |>
    distinct(
      component, consolidated_name, component_size, species = Accepted_species
    )

  ## BIEN
  bien_consolidated = trait_names |>
    # Add species names to trait table
    inner_join(
      bien_traits_simple, by = c(bien_trait_name = "trait_name"),
      relationship = "many-to-many"
    ) |>
    distinct(
      component, consolidated_name, component_size,
      species = scrubbed_species_binomial
    )

  ## GIFT
  gift_consolidated = gift_traits_harmo |>
    inner_join(
      trait_names, by = "gift_trait_name",
      relationship = "many-to-many"
    ) |>
    distinct(
      component, consolidated_name, component_size, species = Accepted_species
    )

  ## TRY
  try_consolidated = trait_names |>
    mutate(try_trait_id = as.integer(try_trait_id)) |>
    inner_join(
      try_traits_harmo, by = c(try_trait_id = "TraitID"),
      relationship = "many-to-many"
    ) |>
    distinct(
      component, consolidated_name, component_size, species = Accepted_species
    )

  glonaf_tnrs = glonaf_tnrs |>
    distinct(species = Accepted_species) |>
    filter(species != "") |>
    mutate(in_glonaf = TRUE)

  full_consolidated = list(
    AusTraits = austraits_consolidated,
    BIEN      = bien_consolidated,
    GIFT      = gift_consolidated,
    TRY       = try_consolidated
  ) |>
    bind_rows(.id = "database") |>
    left_join(glonaf_tnrs, by = "species") |>
    mutate(in_glonaf = ifelse(is.na(in_glonaf), FALSE, TRUE))

}


count_trait_combinations = function(
    combined_traits_all, glonaf_tnrs, match_type
) {

  combs = case_when(match_type == "full" ~ get_full_combs(),
                    match_type == "close" ~ get_close_combs(),
                    match_type == "exact" ~ get_exact_combs())

  combined_traits_all  |>
    filter(species != "") |>
    distinct(consolidated_name, species, in_glonaf) |>
    group_by(species) |>
    summarise(
      traits = list(consolidated_name), in_glonaf = unique(in_glonaf)
    ) |>
    # Add GlonAF species without trait data
    full_join(
      glonaf_tnrs |>
        filter(Accepted_species != "") |>
        distinct(species = Accepted_species) |>
        mutate(in_glonaf = TRUE),
      by = join_by(species, in_glonaf)
    ) |>
    rowwise() |>
    mutate(
      has_at_least_one_trait = length(traits) > 0 & all(!is.na(traits)),
      # Consider that each combinations can have multiple ways of writing it
      # first 'any()' means any combination variant
      # then 'all()' means all traits of given combination variant should be
      # present to consider that combination is present
      has_lhs                = ifelse(
        any(apply(combs$lhs, 1, \(x) all(x %in% traits))), TRUE, FALSE
      ),
      has_diaz               = ifelse(
        any(apply(combs$diaz, 1, \(x) all(x %in% traits))), TRUE, FALSE
      ),
      has_bergmann           = ifelse(
        any(apply(combs$bergmann, 1, \(x) all(x %in% traits))), TRUE, FALSE
      )
    ) |>
    ungroup()

}


get_full_combs = function() {

  # LHS
  full_sla = c(
    "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA) of total leaf area",
    "Leaf mass per area"
  )
  full_height = "Plant vegetative height"
  full_seed_mass = "Seed dry mass"

  full_lhs = tidyr::expand_grid(
    sla = full_sla, height = full_height, seed_mass = full_seed_mass
  )


  # Diaz
  full_leaf_area = "Leaf area"
  full_wood_density = c("Wood density", "Leaf dry matter content (LDMC)")
  full_leaf_n_mass = "Leaf nitrogen (N) content per unit leaf dry mass"

  full_diaz = tidyr::expand_grid(
    full_lhs, leaf_area = full_leaf_area, wood_density = full_wood_density,
    leaf_n_mass = full_leaf_n_mass
  )

  # Bergmann
  full_root_diameter = c(
    "Root diameter", "Coarse root diameter", "Fine root diameter",
    "Fine root (absorptive) diameter", "Fine root (transport) diameter"
  )
  full_root_tissue_density = c(
    "Belowground plant organ tissue density (belowground plant organ dry mass per belowground plant organ",
    "Coarse root tissue density (coarse root dry mass per coarse root volume)",
    "Fine root (absorptive) tissue density (absorptive fine root dry mass per absorptive fine root volume",
    "Fine root (transport) tissue density (transport fine root dry mass per transport fine root volume)",
    "Fine root tissue density (fine root dry mass per fine root volume)",
    "Root dry mass per root volume (root density, root tissue density)"
  )

  full_root_length = c(
    "Specific root length (SRL)",
    "Fine root length per fine root dry mass (specific fine root length, SRL)"
  )
  full_root_nitrogen = c(
    "Root nitrogen (N) content per unit root dry mass",
    "Coarse root nitrogen (N) content per coarse root dry mass",
    "Fine root nitrogen (N) content per fine root dry mass"
  )

  full_bergmann = tidyr::expand_grid(
    root_diameter = full_root_diameter,
    root_tissue_density = full_root_tissue_density,
    root_length = full_root_length,
    root_nitrogen = full_root_nitrogen
  )

  list(lhs = full_lhs, diaz = full_diaz, bergmann = full_bergmann)

}


get_close_combs = function() {

  # LHS
  close_sla = c(
    "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA) of total leaf area",
    "Leaf mass per area"
  )
  close_height = "Plant vegetative height"
  close_seed_mass = "Seed dry mass"

  close_lhs = tidyr::expand_grid(
    sla = close_sla, height = close_height, seed_mass = close_seed_mass
  )

  # Diaz
  close_leaf_area = "Leaf area"
  close_wood_density = c("Wood density", "Leaf dry matter content (LDMC)")
  close_leaf_n_mass = "Leaf nitrogen (N) content per unit leaf dry mass"

  close_diaz = tidyr::expand_grid(
    close_lhs, leaf_area = close_leaf_area, wood_density = close_wood_density
  )

  # Bergmann
  close_root_diameter = c(
    "Root diameter", "Coarse root diameter", "Fine root diameter",
    "Fine root (absorptive) diameter", "Fine root (transport) diameter"
  )
  close_root_tissue_density = c(
    "Belowground plant organ tissue density (belowground plant organ dry mass per belowground plant organ",
    "Coarse root tissue density (coarse root dry mass per coarse root volume)",
    "Fine root (absorptive) tissue density (absorptive fine root dry mass per absorptive fine root volume",
    "Fine root (transport) tissue density (transport fine root dry mass per transport fine root volume)",
    "Fine root tissue density (fine root dry mass per fine root volume)",
    "Root dry mass per root volume (root density, root tissue density)"
  )

  close_root_length = c(
    "Specific root length (SRL)",
    "Fine root length per fine root dry mass (specific fine root length, SRL)"
  )
  close_root_nitrogen = c(
    "Root nitrogen (N) content per unit root dry mass",
    "Coarse root nitrogen (N) content per coarse root dry mass",
    "Fine root nitrogen (N) content per fine root dry mass"
  )

  close_bergmann = tidyr::expand_grid(
    root_diameter = close_root_diameter,
    root_tissue_density = close_root_tissue_density,
    root_length = close_root_length,
    root_nitrogen = close_root_nitrogen
  )

  list(lhs = close_lhs, diaz = close_diaz, bergmann = close_bergmann)

}


get_exact_combs = function() {

  # LHS
  exact_sla = c(
    "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA) of total leaf area",
    "Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA) petiole, rhachis and midrib excluded",
    "Leaf mass per area"
  )
  exact_height = "Plant vegetative height"
  exact_seed_mass = "Seed dry mass"

  exact_lhs = tidyr::expand_grid(
    sla = exact_sla, height = exact_height, seed_mass = exact_seed_mass
  )


  # Diaz
  exact_leaf_area = c(
    "Leaf area",
    "Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole is in- or e",
    "Leaf area (in case of compound leaves: leaf, petiole excluded)",
    "Leaf area (in case of compound leaves: leaf, petiole included)",
    "Leaf area (in case of compound leaves: leaflet, petiole excluded)",
    "Leaf area (in case of compound leaves: leaflet, petiole included)",
    "Leaf area (in case of compound leaves: leaflet, undefined if petiole is in- or excluded)"
  )
  exact_wood_density = c("Wood density", "Leaf dry matter content (LDMC)")
  exact_leaf_n_mass = "Leaf nitrogen (N) content per unit leaf dry mass"

  exact_diaz = tidyr::expand_grid(
    exact_lhs, leaf_area = exact_leaf_area, wood_density = exact_wood_density,
    leaf_n_mass = exact_leaf_n_mass
  )

  # Bergmann
  exact_root_diameter = c(
    "Root diameter", "Coarse root diameter", "Fine root diameter",
    "Fine root (absorptive) diameter", "Fine root (transport) diameter"
  )
  exact_root_tissue_density = c(
    "Belowground plant organ tissue density (belowground plant organ dry mass per belowground plant organ",
    "Coarse root tissue density (coarse root dry mass per coarse root volume)",
    "Fine root (absorptive) tissue density (absorptive fine root dry mass per absorptive fine root volume",
    "Fine root (transport) tissue density (transport fine root dry mass per transport fine root volume)",
    "Fine root tissue density (fine root dry mass per fine root volume)",
    "Root dry mass per root volume (root density, root tissue density)"
  )

  exact_root_length = c(
    "Specific root length (SRL)",
    "Fine root length per fine root dry mass (specific fine root length, SRL)"
  )
  exact_root_nitrogen = c(
    "Root nitrogen (N) content per unit root dry mass",
    "Coarse root nitrogen (N) content per coarse root dry mass",
    "Fine root nitrogen (N) content per fine root dry mass"
  )

  exact_bergmann = tidyr::expand_grid(
    root_diameter = exact_root_diameter,
    root_tissue_density = exact_root_tissue_density,
    root_length = exact_root_length,
    root_nitrogen = exact_root_nitrogen
  )

  list(lhs = exact_lhs, diaz = exact_diaz, bergmann = exact_bergmann)

}


# Function to create a unified growth form dataset to use downstream
standardize_growth_form = function(
    trait_names_full, austraits, bien_traits, gift_raw_traits, gift_trait_meta,
    full_try_df, try_harmonized_species
) {

  growth_form_traits = trait_names_full |>
    filter(consolidated_name == "Plant growth form")

  austraits_growth = austraits$traits |>
    filter(trait_name %in% unique(growth_form_traits$austraits_trait_name)) |>
    distinct(taxon_name, trait_name, value)

  bien_growth = bien_traits |>
    filter(
      !is.na(scrubbed_species_binomial),
      trait_name %in% unique(growth_form_traits$bien_trait_name)
    ) |>
    distinct(scrubbed_species_binomial, trait_name, trait_value)

  gift_growth = gift_raw_traits |>
    semi_join(
      gift_trait_meta |>
        filter(Trait2 %in% unique(growth_form_traits$gift_trait_name)),
      by = c(trait_ID = "Lvl3")
    )

  try_growth = full_try_df |>
    filter(TraitID %in% unique(growth_form_traits$try_trait_id))

  # Filter actually interesting trait
  austraits_growth_simple = austraits_growth |>
    filter(trait_name == "plant_growth_form") |>
    mutate(
      simplified_value = case_when(
        grepl("tree", value, fixed = TRUE)  ~ "tree",
        grepl("shrub", value, fixed = TRUE) ~ "shrub",
        grepl("herb", value, fixed = TRUE)  ~ "herb",
        TRUE                                ~ "other"
      )
    )

  bien_growth_simple = bien_growth |>
    filter(trait_name == "whole plant growth form")

  gift_growth_simple = gift_growth |>
    filter(trait_ID == "1.2.1") |>
    distinct(work_species, trait_value)

  try_growth_simple = try_growth |>
    select(
      AccSpeciesID, AccSpeciesName, TraitID, TraitName, Dataset, OriglName,
      OrigValueStr, StdValue
    ) |>
    inner_join(
      try_harmonized_species |>
        select(
          AccSpeciesID = TRY_AccSpeciesID, AccSpeciesName = TRY_AccSpeciesName,
          MatchedName
        ),
      by = join_by(AccSpeciesID, AccSpeciesName)
    )

  austraits_list = austraits_growth_simple |>
    distinct(taxon_name, simplified_value) |>
    group_by(taxon_name) |>
    summarise(growth_form = list(unique(simplified_value)))

  bien_list = bien_growth_simple |>
    distinct(
      taxon_name       = scrubbed_species_binomial,
      simplified_value = trait_value
    ) |>
    mutate(
      simplified_value = case_when(
        grepl("tree", simplified_value, fixed = TRUE)  |
          grepl("Tree", simplified_value, fixed = TRUE)  ~ "tree",
        grepl("shrub", simplified_value, fixed = TRUE) |
          grepl("Shrub", simplified_value, fixed = TRUE) ~ "shrub",
        grepl("herb", simplified_value, fixed = TRUE)  |
          grepl("Herb", simplified_value, fixed = TRUE) ~ "herb",
        TRUE                                  ~ "other"
      )
    )|>
    group_by(taxon_name) |>
    summarise(growth_form = list(unique(simplified_value)))

  gift_list = gift_growth_simple |>
    distinct(
      taxon_name      = work_species,
      simplified_value =  trait_value
    ) |>
    mutate(
      simplified_value = case_when(
        grepl("tree", simplified_value, fixed = TRUE)  |
          grepl("Tree", simplified_value, fixed = TRUE)  ~ "tree",
        grepl("shrub", simplified_value, fixed = TRUE) |
          grepl("Shrub", simplified_value, fixed = TRUE) ~ "shrub",
        grepl("herb", simplified_value, fixed = TRUE)  |
          grepl("Herb", simplified_value, fixed = TRUE) ~ "herb",
        TRUE                                  ~ "other"
      )
    ) |>
    group_by(taxon_name) |>
    summarise(growth_form = list(unique(simplified_value)))

  try_list = try_growth_simple |>
    filter(TraitID == 42) |>
    distinct(
      taxon_name       = MatchedName,
      simplified_value = OrigValueStr
    ) |>
    mutate(
      simplified_value = case_when(
        grepl("tree", simplified_value, fixed = TRUE)  |
          grepl("Tree", simplified_value, fixed = TRUE)  ~ "tree",
        grepl("shrub", simplified_value, fixed = TRUE) |
          grepl("Shrub", simplified_value, fixed = TRUE) ~ "shrub",
        grepl("herb", simplified_value, fixed = TRUE)  |
          grepl("Herb", simplified_value, fixed = TRUE) ~ "herb",
        TRUE                                  ~ "other"
      )
    ) |>
    group_by(taxon_name) |>
    summarise(growth_form = list(unique(simplified_value)))


  total_growth_form = list(austraits = austraits_list,
                           bien      = bien_list,
                           gift      = gift_list,
                           try       = try_list) |>
    bind_rows() |>
    group_by(taxon_name) |>
    summarise(growth_form = list(unique(unlist(growth_form)))) |>
    rowwise() |>
    mutate(simplified_growth_form = case_when(
      "tree" %in% growth_form  ~ "tree",
      "shrub" %in% growth_form ~ "shrub",
      "herb" %in% growth_form  ~ "herb",
      TRUE                     ~ "other"
    )) |>
    ungroup()

}

# Count Summary of Number of Traits per species per Region
count_number_of_traits_per_region = function(
    glonaf_species_regions, combined_traits
) {

  match_type = names(combined_traits)

  combined_traits = combined_traits[[1]]

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
    ) |>
    mutate(match_type = match_type)
}

list_species_by_trait_per_database = function(combined_traits_origin) {

  combined_traits_origin %>%
    ungroup() %>%
    distinct(consolidated_name, database, species) |>
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
    select(consolidated_name, database, sp_list, n_all) %>%
    # Get all combinations of trait name and database
    tidyr::complete(
      consolidated_name, database, fill = list(sp_list = list(""))
    ) %>%
    # Fill NAs 'n_all' columns by non empty values
    group_by(consolidated_name) %>%
    arrange(desc(n_all), consolidated_name, database) %>%
    mutate(n_all = zoo::na.locf(n_all)) %>%
    ungroup() %>%
    # Organize diagrams per number of species/trait name/database name
    arrange(desc(n_all), consolidated_name, database) %>%
    group_by(consolidated_name) %>%
    # Create actual Euler diagrams
    summarise(
      euler = sp_list %>%
        setNames(database) %>%
        eulerr::euler() %>%
        lst(),
      n_all = unique(n_all)
    ) %>%
    arrange(desc(n_all))

}
