# Try to identify Georeferenced traits
tar_load(austraits)
tar_load(glonaf_bien_traits)
tar_load(full_try_df)

# AusTraits
aus_coords = austraits$sites %>%
  filter(grepl("latitude", site_property) | grepl("longitude", site_property))
aus_traits_with_coords = austraits$traits %>%
  full_join(
    aus_coords %>%
              select(dataset_id, site_name) %>%
              mutate(has_coords = TRUE),
    by = c("dataset_id", "site_name")
  ) %>%
  mutate(has_coords = ifelse(is.na(has_coords), FALSE, has_coords)) %>%
  distinct(taxon_name, trait_name, has_coords)


# BIEN
bien_traits_with_coords = glonaf_bien_traits %>%
  distinct(scrubbed_species_binomial, trait_name, latitude, longitude) %>%
  mutate(has_coords = !is.na(latitude) & !is.na(longitude)) %>%
  select(-latitude, -longitude)

# TRY
try_coords_data = full_try_df %>%
  select(DataID, DataName, OriglName) %>%
  collect() %>%
  distinct() %>%
  filter(
    grepl(".*longitude.*", DataName, ignore.case = TRUE) |
      grepl(".*latitude.*", DataName, ignore.case = TRUE)
  ) %>%
  distinct(DataID, DataName) %>%
  # Remove coordinates that for sure
  filter(
    !grepl("provenance", DataName, fixed = TRUE) &
      !grepl("Seed origin", DataName, fixed = TRUE)
  )

# Consider that trait has coordinates when dataset has at least coordinates
try_trait_with_coords =
  full_try_df %>%
  full_join(
    full_try_df %>%
  filter(DataID %in% c(59, 60)) %>%  # Latitude and longitude
  collect() %>%
  distinct(DatasetID, has_coords = TRUE),
  by = "DatasetID") %>%
  mutate(has_coords = ifelse(is.na(has_coords, FALSE, has_coords)))
