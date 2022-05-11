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
    inner_join(tbl(glonaf_con, "name_status"), by = c(name_status_id = "id")) %>%
    select(-name_status_id) %>%
    # Get only binomial name
    filter(infra_rank_id == 4) %>%
    # Add full genus name
    inner_join(glonaf_con %>%
                 tbl("genus") %>%
                 select(genus_id = id, genus = name), by = "genus_id") %>%
    select(-genus_id) %>%
    # Add author name
    inner_join(glonaf_con  %>%
                 tbl("author") %>%
                 select(author_id = id, author_name = name),
               by = "author_id") %>%
    select(-author_id) %>%
    collect()

  discon(glonaf_con)

  return(species_list)
}

extract_glonaf_list = function(glonaf_alien_species) {
  glonaf_alien_species %>%
    filter(name == "accepted") %>%
    distinct(genus, epithet, author_name) %>%
    mutate(full_name = paste(genus, epithet, author_name)) %>%
    pull(full_name) %>%
    unique() %>%
    iconv("utf-8", "latin1")
}

get_glonaf_higher_taxonomy_combined_traits = function(
  combined_traits, match_glonaf_tnrs, glonaf_alien_species
) {

  glonaf_con = connect_glonaf_db()

  combined_traits %>%
    distinct(species) %>%
    full_join(
      match_glonaf_tnrs %>%
        select(Name_submitted, Name_matched) %>%
        mutate(Name_matched = iconv(Name_matched, "utf-8", "latin1")),
      by = c(species = "Name_matched")
    ) %>%
    full_join(
      glonaf_alien_species %>%
        select(tpl_id, genus, epithet, author_name) %>%
        mutate(Name_submitted = paste(genus, epithet, author_name) %>%
                 iconv("utf-8", "latin1")),
      by = "Name_submitted"
    ) %>%
    left_join(
      tbl(glonaf_con, "genus") %>%
        select(genus_id = id, genus = name, family_tpl_id) %>%
        collect(),
      by = "genus"
    ) %>%
    left_join(
      tbl(glonaf_con, "family_tpl") %>%
        rename(family_tpl_id = id, family = name) %>%
        collect(),
      by = "family_tpl_id"
    ) %>%
    left_join(
      tbl(glonaf_con, "family_apg") %>%
        rename(family_apg_id = id, family_apg = name) %>%
        collect(),
      by = "family_apg_id"
    ) %>%
    left_join(
      tbl(glonaf_con, "order_apg") %>%
        rename(order_id = id, order = name) %>%
        collect(),
      by = "order_id"
    )
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
