# Try to identify Georeferenced traits
tar_load(austraits)
tar_load(glonaf_bien_traits)
tar_load(full_try_df)

# AusTraits
aus_coords = austraits$sites %>%
  filter(grepl("latitude", site_property) | grepl("longitude", site_property))
aus_traits_with_coords = austraits$traits %>%
  semi_join(aus_coords, by = c("dataset_id", "site_name"))


# BIEN
bien_traits_with_coords = glonaf_bien_traits %>%
  filter(!is.na(latitude), !is.na(longitude))

# GIFT
# Complicated

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
try_trait_with_coords = full_try_df %>%
  select(DatasetID, DataID) %>%
  collect() %>%
  distinct() %>%
  semi_join(try_coords_data, by = c("DataID"))
