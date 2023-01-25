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
  species_count = tbl(glonaf_con, "flora_orig") %>%
    select(taxon_orig_id, status_id) %>%
    distinct() %>%
    count(status_id, sort = TRUE) %>%
    inner_join(tbl(glonaf_con, "status"), by = c("status_id" = "id")) %>%
    collect()

  discon(glonaf_con)

  return(species_count)
}

get_glonaf_species_list = function(glonaf_con) {
  species_list = tbl(glonaf_con, "flora_orig") %>%
    # Get taxa that are referenced as naturalized, alien, or invasive
    filter(status_id %in% c(2, 4, 5, 7)) %>%
    distinct(taxon_orig_id) %>%
    # Get species names and ids
    inner_join(tbl(glonaf_con, "taxon_orig"), by = c(taxon_orig_id = "id")) %>%
    # Corrected names after matching TPL
    distinct(species_id) %>%
    inner_join(tbl(glonaf_con, "species"), by = c(species_id = "id")) %>%
    select(-species_id) %>%
    # Add Name status from TPL
    inner_join(
      tbl(glonaf_con, "name_status"),
      by = c(name_status_id = "id")
    ) %>%
    select(-name_status_id) %>%
    # Get only binomial name
    filter(infra_rank_id == 4) %>%
    # Add full genus name
    inner_join(
      tbl(glonaf_con, "genus") %>%
        select(genus_id = id, genus = name),
      by = "genus_id"
    ) %>%
    select(-genus_id) %>%
    # Add author name
    inner_join(
      tbl(glonaf_con, "author") %>%
        select(author_id = id, author_name = name),
      by = "author_id"
    ) %>%
    select(-author_id) %>%
    collect()

  discon(glonaf_con)

  return(species_list)
}

extract_glonaf_list = function(glonaf_alien_species) {
  glonaf_alien_species %>%
    distinct(genus, epithet, author_name) %>%
    mutate(full_name = paste(genus, epithet, author_name)) %>%
    pull(full_name) %>%
    unique()
}

get_glonaf_higher_taxonomy_combined_traits = function(match_glonaf_tnrs) {

  match_glonaf_tnrs %>%
    distinct(species = Accepted_species, family = Accepted_family) %>%
    filter(species != "") %>%
    mutate(genus = stringr::word(species, 1)) %>%
    select(species, genus, family)

}


get_glonaf_region_correspondence = function(glonaf_alien_species) {

  glonaf_con = connect_glonaf_db()

  ## Get a list of species <-> region ids
  glonaf_con %>%
    ## Get list of species
    tbl("flora_orig") %>%
    filter(status_id %in% c(2, 4, 5, 7)) %>%
    distinct(taxon_orig_id) %>%
    # Get species names and ids
    inner_join(tbl(glonaf_con, "taxon_orig"), by = c(taxon_orig_id = "id")) %>%
    collect() %>%
    ## Merge extracted species
    inner_join(
      glonaf_alien_species,
      by = c("genus", "epithet", "hybrid", "epithet_infra")
    ) %>%
    ## Get region ids
    inner_join(
      glonaf_con %>%
        tbl("flora_orig") %>%
        filter(status_id %in% c(2, 4, 5, 7)) %>%
        distinct(list_id, taxon_orig_id) %>%
        inner_join(
          glonaf_con %>%
            tbl("list") %>%
            select(list_id = id, region_id),
          by = "list_id"
        ) %>%
        inner_join(
          glonaf_con %>%
            tbl("region") %>%
            distinct(region_id = id, region_name = name, OBJIDsic)
        ) %>%
        collect()
    ) %>%
    select(taxon_orig_id, taxon_orig, taxon_corrected, genus, epithet, hybrid,
           author_name, OBJIDsic)
}

get_glonaf_species_number = function(glonaf_con) {
  sp = glonaf_con %>%
    tbl("species_numbers") %>%
    select(OBJIDsic, starts_with("num_")) %>%
    collect()

  discon(glonaf_con)

  return(sp)
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
    glonaf_con, match_glonaf_tnrs, glonaf_list
) {

  # Reconstruct GloNAF df
  glonaf_names_df = data.frame(
    id           = paste0("glonaf-", seq_along(glonaf_list)),
    species_name = glonaf_list
  )

  # Extract all regions for all species referenced in GloNAF
  species_regions = tbl(glonaf_con, "flora_orig") %>%
    # Get taxa that are referenced as naturalized, alien, or invasive
    filter(status_id %in% c(2, 4, 5, 7)) %>%
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
    # Get species names and ids
    inner_join(tbl(glonaf_con, "taxon_orig"), by = c(taxon_orig_id = "id")) %>%
    # Corrected names after matching TPL
    distinct(species_id, taxon_orig_id, status_name, OBJIDsic) %>%
    inner_join(tbl(glonaf_con, "species"), by = c(species_id = "id")) %>%
    select(-species_id) %>%
    # Add Name status from TPL
    inner_join(tbl(glonaf_con, "name_status"), by = c(name_status_id = "id")) %>%
    select(-name_status_id) %>%
    # Get only binomial name
    filter(infra_rank_id == 4) %>%
    # Add full genus name
    inner_join(
      glonaf_con %>%
        tbl("genus") %>%
        select(genus_id = id, genus = name), by = "genus_id"
    ) %>%
    select(-genus_id) %>%
    # Add author name
    inner_join(
      glonaf_con  %>%
        tbl("author") %>%
        select(author_id = id, author_name = name),
      by = "author_id"
    ) %>%
    select(-author_id) %>%
    collect()

  discon(glonaf_con)
  # Merge this information based on
  species_regions %>%
    distinct(OBJIDsic, status_name, genus, epithet, author_name) %>%
    mutate(
      full_name = paste(genus, epithet, author_name)
    ) %>%
    inner_join(
      match_glonaf_tnrs %>%
        inner_join(
          glonaf_names_df,
          by = c(ID = "id")
        ) %>%
        distinct(species_name, species = Accepted_species),
      by = c(full_name = "species_name")
    ) %>%
    distinct(species, status_name, OBJIDsic) %>%
    select(OBJIDsic, status_name, species) %>%
    filter(species != "")
}

count_species_proportion_trait_by_region = function(
    glonaf_species_regions, contain_trait_combination
) {

  glonaf_species_regions %>%
    full_join(
      contain_trait_combination %>%
        ungroup() %>%
        filter(species != "") %>%
        select(species, has_at_least_one_trait :has_bergmann) %>%
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
    )

}


#' Merge GloNAF status with trait categories and combinations
#'
#' @noRd
get_trait_combinations_and_cat_per_invasion_status = function(
    glonaf_species_regions_status, contain_trait_combination
) {
  glonaf_species_regions_status %>%
    distinct(species, status_name) %>%
    mutate(
      status_name = ifelse(
        status_name == "naturalized_archeophyte", "naturalized", status_name
      )
    ) %>%
    inner_join(
      contain_trait_combination %>%
        mutate(n_traits = length(traits)) %>%
        select(-traits, -in_glonaf),
      by = "species")
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

count_most_distributed_species_and_bootstrap = function(
    glonaf_species_area, n_top = 100, n_bootstrap = 100
) {

  glonaf_species_area %>%
    tidyr::pivot_longer(
      !species, names_to = "area_type", values_to = "area_value"
    ) %>%
    tidyr::nest(area_df = !area_type) %>%
    mutate(
      top_species = purrr::map(
        area_df, ~slice_max(.x, area_value, n = n_top) %>%
          mutate(boot_name = "top")
      ),
      rest_df = purrr::map2(
        area_df, top_species, ~anti_join(.x, .y, by = "species")
      ),
      bootstrap_df = purrr::map(
        rest_df,
        ~lapply(seq(n_bootstrap), function(x) slice_sample(.x, n = n_top)) %>%
          bind_rows() %>%
          mutate(boot_name = "bootstrap")
      )
    ) %>%
    select(-area_df, -rest_df)
}


get_european_regions_of_glonaf = function(glonaf_mainland_large_islands) {

  europe_sf = rnaturalearth::ne_countries(returnclass = "sf") %>%
    sf::st_transform(crs = "EPSG:4258") %>%
    filter(continent == "Europe")

  europe_sf %>%
    sf::st_transform(sf::st_crs(glonaf_mainland_large_islands)) %>%
    sf::st_join(
      glonaf_mainland_large_islands %>%
        select(OBJIDsic, IDregion, regionF, name),
      left = TRUE
    ) %>%
    ## Manually filter out some regions
    # Filter out non-European Russian Regions
    filter(!grepl("RUS.RFE|RUS.KI|RUS.MAG|RUS.SIB|RUS.KHA", IDregion)) %>%
    # Filter out Matched South American Regions
    filter(!grepl("BRA|SUR", IDregion)) %>%
    # Filter out Matched other Asian regions
    filter(!grepl("KAZ|CHN|MNG", IDregion))

}

get_european_glonaf_species = function(
    glonaf_europe, glonaf_species_regions, contain_trait_combination
) {
  glonaf_species_regions %>%
    semi_join(glonaf_europe, by = "OBJIDsic") %>%
    distinct(OBJIDsic, species) %>%
    count(species, name = "n_regions") %>%
    full_join(
      contain_trait_combination %>%
        select(-in_glonaf),
      by = "species"
    ) %>%
    rowwise() %>%
    mutate(
      has_seed_mass = "seed mass" %in% traits,
      has_sla       = "leaf area per leaf dry mass" %in% traits,
      has_height    = "whole plant height" %in% traits
    ) %>%
    ungroup() %>%
    arrange(desc(n_regions)) %>%
    mutate(
      occurrence_rank = row_number()
    )
}
