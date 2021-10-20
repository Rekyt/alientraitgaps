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
    # Get taxa that are referenced as naturalized, aliens, or invasive
    filter(status_id %in% c(2, 4, 5, 7)) %>%
    distinct(taxon_orig_id) %>%
    # Get species names and ids
    inner_join(tbl(glonaf_con, "taxon_orig"), by = c("taxon_orig_id" = "id")) %>%
    # Corrected names after matching TPL
    distinct(species_id) %>%
    inner_join(tbl(glonaf_con, "species"), by = c("species_id" = "id")) %>%
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
    unique()
}
