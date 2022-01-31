library("dplyr")
library("targets")
library("sf")
library("ggplot2")

tar_load(c("glonaf_regions", "glonaf_regions_list"))

glonaf_con = connect_glonaf_db()

fine_regions = tbl(glonaf_con, "region") %>%
  # Get biggest
  filter(finest_complete_resolution == 1 | OBJIDsic %in% c(1194)) %>%
  distinct(id, code, name, parent_id, OBJIDsic) %>%
  collect()

discon(glonaf_con)

glonaf_regions %>%
  semi_join(fine_regions, by = "OBJIDsic") %>%
  .["OBJIDsic"] %>%
  plot()

fine_yes = glonaf_regions %>%
  semi_join(
    tbl(glonaf_con, "region") %>%
      # Get biggest
      filter(finest_complete_resolution == 1) %>%
      collect(),
  by = "OBJIDsic")

fine_no = glonaf_regions %>%
  semi_join(
    tbl(glonaf_con, "region") %>%
      # Get biggest
      filter(finest_complete_resolution == 0) %>%
      collect(),
    by = "OBJIDsic")

ko = fine_no %>%
  sf::st_transform("+proj=eck4") %>%
  st_join(fine_yes %>%
            sf::st_transform("+proj=eck4"),
          join = st_covers, left = TRUE)
