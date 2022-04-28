library("targets")
library("dplyr")
library("ggplot2")

tar_load(c("glonaf_regions_list", "glonaf_regions"))

glonaf_con = connect_glonaf_db()

only_regions_sf = glonaf_regions %>%
  inner_join(
    glonaf_con %>%
      tbl("region") %>%
      collect(),
    by = c("OBJIDsic", "name")
  ) %>%
  sf::st_transform(crs = "+proj=eck4")


world_sf = rnaturalearth::ne_countries(returnclass = "sf") %>%
  sf::st_transform(crs = "+proj=eck4")

plot_regions = only_regions_sf %>%
  bind_rows(
    modif_regions %>%
      mutate(OBJIDsic = as.numeric(OBJIDsic),
             finest_complete_resolution = 3) %>%
      sf::st_transform(crs = "+proj=eck4")
  ) %>%
  ggplot() +
  geom_sf(data = world_sf, fill = "gray85", color = "gray55") +
  geom_sf(aes(fill = as.factor(finest_complete_resolution))) +
  facet_wrap(vars(finest_complete_resolution), ncol = 1)

ggsave("test.pdf", plot_regions, width = 183, units = "mm")


complete_region_world = only_regions_sf %>%
  filter(
    finest_complete_resolution == 1 |
      # Add missing country
      OBJIDsic %in% c(
        329,   # Oman
        377,   # European Part of Russia
        439,   # Syria
        925,   # Japan
        1194,  # Sudan
        1203   # Somalia
      ) |
      # Add all regions from country mostly empty
      grepl("^NOR", IDregion) |  # Keep all regions of Norway
      grepl("^PAN", IDregion) |  # Keep all regions of Panama
      # Add country to fill up missing regions
      OBJIDsic %in% c(
        25,    # Argentina
        52,    # Brazil
        85,    # Chile
        354,   # Paraguay
        662,   # Bolivia
        815,   # Colombia
        831,   # Ecuador
        1068,  # Peru
        1267   # Uruguay
      )

  )
complete_regions = complete_region_world %>%
  ggplot() +
  geom_sf(data = world_sf, fill = "gray85", color = "gray55", size = 1/2) +
  geom_sf(fill = "darkblue", alpha = 1/3, size = 1/5) +
  theme_void()

ggsave("complete_regions.pdf", complete_regions, width = 183, units = "mm")

only_regions_sf %>%
  filter(grepl("^CHL", IDregion)) %>%
  ggplot() +
  geom_sf(data = world_sf %>%
            filter(su_a3 == "CHL"), fill = "gray85", color = "gray55") +
  geom_sf(aes(fill = as.factor(finest_complete_resolution))) +
  facet_wrap(vars(finest_complete_resolution), ncol = 1)

ko = only_regions_sf %>%
  filter(grepl("^RUS", IDregion)) %>%
  filter(IDregion == "RUS.EUR") %>%
  ggplot() +
  geom_sf(data = world_sf %>%
            filter(su_a3 == "RUS"), fill = "gray85", color = "gray55") +
  geom_sf(aes(fill = as.factor(finest_complete_resolution))) +
  facet_wrap(vars(finest_complete_resolution), ncol = 1)

ggsave("test.pdf", ko, width = 183, units = "mm")
