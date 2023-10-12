# Functions to gather (and count) traits with geographical coordinates

# AusTraits
get_austraits_traits_with_coords = function(austraits) {

  aus_coords = austraits$sites %>%
    filter(grepl("latitude", site_property) | grepl("longitude", site_property))

  austraits$traits %>%
    full_join(
      aus_coords %>%
        select(dataset_id, site_name) %>%
        mutate(has_coords = TRUE),
      by = c("dataset_id", "site_name")
    ) %>%
    mutate(has_coords = ifelse(is.na(has_coords), FALSE, has_coords)) %>%
    distinct(taxon_name, trait_name, has_coords)
}

# BIEN
get_bien_traits_with_coords = function(glonaf_bien_traits) {

  glonaf_bien_traits %>%
    distinct(scrubbed_species_binomial, trait_name, latitude, longitude) %>%
    mutate(has_coords = !is.na(latitude) & !is.na(longitude)) %>%
    select(-latitude, -longitude) %>%
    distinct(scrubbed_species_binomial, trait_name, has_coords)

}

# TRY
get_try_traits_with_coords = function(full_try_df) {

  full_try_df %>%
    collect() %>%
    # Add column has_coords
    full_join(
      # Get list of datasets with coordinates
      full_try_df %>%
        filter(DataID %in% c(59, 60)) %>%  # Latitude and longitude
        chunk_distinct(DatasetID, has_coords = TRUE) %>%
        collect() %>%
        distinct(DatasetID, has_coords),
      by = "DatasetID") %>%
    mutate(has_coords = ifelse(is.na(has_coords), FALSE, has_coords)) %>%
    distinct(AccSpeciesID, AccSpeciesName, TraitID, TraitName, has_coords)

}
