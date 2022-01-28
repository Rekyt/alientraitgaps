# Script to extract taxonomic information back
library("targets")
library("dplyr")

# Load database
glonaf_con = connect_glonaf_db()

ko = sf::read_sf("../fdresistance/inst/exdata/glonaf/regions_2020-10-28/regions_2020-10-28.shp")

tbl(glonaf_con, "flora_orig") %>%
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
      distinct(region_id = id, region_name = name, OBJIDisc)
  )
