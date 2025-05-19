connect_glonaf_db = function() {
  glonaf_con = DBI::dbConnect(
    RMariaDB::MariaDB(),
    dbname = "glonafdb",
    username = Sys.getenv("GLONAF_USER"),
    password = Sys.getenv("GLONAF_PASSWORD"),
    host = "134.34.205.46",
    port = 6609
  )
}

discon = function(con) {
  DBI::dbDisconnect(con)
}

get_glonaf_alien_species_count = function(glonaf_con) {

  species_count = tbl(glonaf_con, "flora_orig_2_0") %>%
    select(taxon_orig_id, status_id) %>%
    distinct() %>%
    count(status_id, sort = TRUE) %>%
    inner_join(tbl(glonaf_con, "status"), by = c("status_id" = "id")) %>%
    collect()

  discon(glonaf_con)

  return(species_count)
}

get_glonaf_species_list = function(glonaf_con) {

  species_list = tbl(glonaf_con, "flora_orig_2_0") %>%
    # Get taxa that are referenced as naturalized or invasive
    filter(status_id %in% c(2, 5)) %>%
    # Keep only lists that are not outdated
    semi_join(
      tbl(glonaf_con, "list") %>%
        select(list_id = id, outdated) %>%
        filter(outdated == 0),
      by = "list_id"
    ) %>%
    distinct(taxon_wcvp_id, taxon_orig_id) %>%
    # Get species names
    inner_join(
      tbl(glonaf_con, "taxon_wcvp"),
      by = c("taxon_orig_id", "taxon_wcvp_id" = "id")
    ) %>%
    inner_join(
      tbl(glonaf_con, "species_list_2_0") %>%
        select(accepted_plant_name_id, taxon_status, lifeform, climate, hybrid),
      by = "accepted_plant_name_id"
    ) %>%
    collect()

  discon(glonaf_con)

  return(species_list)
}


#' Create a unified regions sf object
#'
#' There are no standard ways of creating maps in GloNAF. So we decided it
#' to unify a dataset ourself. Specifically when considering finnest possible
#' regions some regions are missing. This function takes into input the sf data
#' of spatial regions with some selections from the GloNAF database to obtain
#' a list of actual regions used for mapping.
#'
#' @noRd
unify_glonaf_regions = function(glonaf_regions) {

  glonaf_con = connect_glonaf_db()

  # Keep all referenced regions from the spatial file given the database
  all_referenced_regions = glonaf_regions %>%
    inner_join(
      glonaf_con %>%
        tbl("region") %>%
        collect(),
      by = c("OBJIDsic", "name")
    ) %>%
    sf::st_transform(crs = "+proj=eqearth")

  discon(glonaf_con)

  # Selected regions
  all_referenced_regions %>%
    filter(
      # Keep finest regions
      finest_complete_resolution == 1 |
        # Add missing countries
        OBJIDsic %in% c(
          25,  # Argentina
          52,  # Brazil
          85,  # Chile
          329,  # Oman
          354,  # Paraguay
          377,  # European Part of Russia
          439,  # Syria
          662,  # Bolivia
          815,  # Colombia
          831,  # Ecuador
          925,  # Japan
          1068,  # Peru
          1194,  # Sudan
          1203,  # Somalia
          1267   # Uruguay
        ) |
        # Add all regions from country mostly empty
        grepl("^NOR", IDregion) |  # Keep all regions of Norway
        grepl("^PAN", IDregion)    # Keep all regions of Panama
    )
}

select_glonaf_small_islands = function(unified_glonaf_regions, area = 2.5e3) {
  unified_glonaf_regions %>%
    filter(island == 1, GeodAREA <= area) %>%
    sf::st_centroid()
}

select_glonaf_mainland_large_islands = function(
    unified_glonaf_regions, glonaf_small_islands
) {
  unified_glonaf_regions %>%
    anti_join(
      glonaf_small_islands %>%
        as.data.frame() %>%
        select(OBJIDsic),
      by = "OBJIDsic"
    )
}


extract_species_regions_table = function(
    glonaf_con, glonaf_alien_species
) {

  # Extract all regions for all species referenced in GloNAF
  species_regions = tbl(glonaf_con, "flora_orig_2_0") %>%
    # Get taxa that are referenced as naturalized, alien, or invasive
    filter(status_id %in% c(2, 5)) %>%
    distinct(taxon_orig_id, list_id, status_id) %>%
    # Add status name
    inner_join(
      tbl(glonaf_con, "status") %>%
        rename(status_id = id,
               status_name = name),
      by = "status_id"
    ) %>%
    select(-status_id) %>%
    # Get list-correspondence
    inner_join(
      tbl(glonaf_con, "list") %>%
        rename(list_id = id) %>%
        distinct(list_id, region_id),
      by = "list_id"
    ) %>%
    select(-list_id) %>%
    distinct() %>%
    # Get region-shapefile polygon correspondence
    inner_join(
      tbl(glonaf_con, "region") %>%
        rename(region_id = id) %>%
        distinct(region_id, OBJIDsic),
      by = "region_id"
    ) %>%
    distinct(taxon_orig_id, status_name, OBJIDsic) %>%
    collect() |>
    inner_join(
      glonaf_alien_species |>
        distinct(taxon_orig_id, species = taxa_accepted),
      by = "taxon_orig_id"
    )

  discon(glonaf_con)

  species_regions
}

count_species_proportion_trait_by_region = function(
    glonaf_species_regions, contain_trait_combination
) {

  match_type = names(contain_trait_combination)

  contain_trait_combination = contain_trait_combination[[1]]

  glonaf_species_regions |>
    full_join(
      contain_trait_combination %>%
        select(species, has_at_least_one_trait:has_bergmann) %>%
        mutate(is_present = TRUE),
      by = "species"
    ) %>%
    mutate(
      across(where(is.logical), ~ifelse(is.na(.x), FALSE, .x))
    ) %>%
    filter(!is.na(OBJIDsic)) %>%
    group_by(OBJIDsic) %>%
    summarise(
      # Proportion of species with given combination
      across(where(is.logical), list(prop = ~sum(.x, na.rm = TRUE)/n())),
      n_species = n()
    ) %>%
    rename(
      prop_with_any_trait = has_at_least_one_trait_prop
    ) |>
    mutate(match_type = match_type)

}


#' Merge GloNAF status with trait categories and combinations
#'
#' @noRd
get_trait_combinations_and_cat_per_invasion_status = function(
    glonaf_species_regions_status, contain_trait_combination
) {

  match_type = names(contain_trait_combination)

  contain_trait_combination = contain_trait_combination[[1]]

  glonaf_species_regions_status %>%
    mutate(
      status_name = ifelse(
        status_name == "naturalized_archeophyte", "naturalized", status_name
      )
    ) %>%
    count(species, status_name) %>%
    tidyr::complete(species, status_name, fill = list(n = 0)) %>%
    arrange(species, status_name) %>%
    tidyr::pivot_wider(names_from = status_name, values_from = n) %>%
    right_join(
      contain_trait_combination %>%
        mutate(n_traits = length(traits)) %>%
        select(-traits, -in_glonaf),
      by = "species"
    ) |>
    mutate(
      match_type  = match_type,
      invasive    = ifelse(is.na(invasive), 0, invasive),
      naturalized = ifelse(is.na(naturalized), 0, naturalized)
    )

}

count_number_of_regions_and_area = function(
    glonaf_species_regions, unified_glonaf_regions
) {
  glonaf_species_regions %>%
    inner_join(
      unified_glonaf_regions %>%
        as.data.frame() %>%
        select(OBJIDsic, GeodAREA),
      by = "OBJIDsic"
    ) %>%
    group_by(species) %>%
    summarise(n_regions = n(), total_area = sum(GeodAREA))
}

get_glonaf_family_genus_species = function(glonaf_tnrs) {

  glonaf_tnrs |>
    distinct(species = Accepted_species, family = Accepted_family) |>
    filter(species != "") |>
    mutate(
      genus   = stringr::str_extract(species, "\\w+"),
      species = stringr::str_replace(species, stringr::fixed(" "), "_")
    ) |>
    select(family, genus, species)

}

build_glonaf_phylo_tree = function(glonaf_species_fam) {

  rtrees::get_tree(
    glonaf_species_fam, taxon = "plant", scenario = "at_basal_node"
  )

}

