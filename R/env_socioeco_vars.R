get_socioeco_variables = function() {
  WDI::WDI(
    indicator = c(
      research_expenditure_perc_gdp = "GB.XPD.RSDV.GD.ZS",
      gdp_total                     = "NY.GDP.MKTP.KD",
      gdp_per_capita                = "NY.GDP.PCAP.KD"
    )
  )
}

average_country_socioeco_vars = function(world_socioeco) {

  world_socioeco %>%
    group_by(country, iso2c, iso3c) %>%
    summarise(
      across(
        research_expenditure_perc_gdp:gdp_per_capita, ~mean(.x, na.rm = TRUE)
      )
    ) %>%
    ungroup()

}


extract_road_density_glonaf_regions = function(
  unified_glonaf_regions, road_density_file
) {

  # Load raster
  road_density_raster = terra::rast(road_density_file)

  # Get average road density across all GloNAF polygons
  road_density_avg = terra::extract(
    road_density_raster,
    unified_glonaf_regions %>%
      sf::st_transform(
        sf::st_crs(road_density_raster)
      ),
    fun = mean, na.rm = TRUE, bind = TRUE
  )

  # Return simplified data.frame
  road_density_avg %>%
    as.data.frame() %>%
    select(OBJIDsic, IDregion, road_density = grip4_total_dens_m_km2)

}

get_world_bank_indicators_on_glonaf = function(
    avg_socioeco, unified_glonaf_regions
) {

  # Get a map of the world
  world_sf = rnaturalearth::ne_countries(returnclass = "sf")

  # Add World Bank Indicators
  world_sf = world_sf %>%
    inner_join(
      avg_socioeco %>%
        select(iso3c:gdp_per_capita),
      by = c(iso_a3 = "iso3c")
    )

  # Combine WDI with Unified
  unified_glonaf_regions %>%
    sf::st_join(world_sf %>%
                  sf::st_transform(sf::st_crs(unified_glonaf_regions))) %>%
    as.data.frame() %>%
    select(OBJIDsic, IDregion, research_expenditure_perc_gdp:gdp_per_capita) %>%
    group_by(OBJIDsic, IDregion) %>%
    summarise(
      across(
        research_expenditure_perc_gdp:gdp_per_capita, ~mean(.x, na.rm = TRUE)
      )
    ) %>%
    ungroup()
}

extract_pop_count_glonaf_regions = function(
  unified_glonaf_regions, population_count_file
) {

  pop_count = terra::rast(population_count_file)

  glonaf_pop_count = terra::extract(
    pop_count[[1:5]],
    unified_glonaf_regions %>%
      sf::st_transform(sf::st_crs(pop_count)),
    fun = mean, na.rm = TRUE
  )

  colnames(glonaf_pop_count)[2:6] = paste0("pop_count", seq(2000, 2020, by = 5))

  unified_glonaf_regions %>%
    as.data.frame() %>%
    select(OBJIDsic, IDregion) %>%
    cbind(glonaf_pop_count[, 2:6])
}

extract_pop_density_glonaf_regions = function(
  unified_glonaf_regions, population_density_file
) {

  pop_density = terra::rast(population_density_file)

  glonaf_pop_density = terra::extract(
    pop_density[[1:5]],
    unified_glonaf_regions %>%
      sf::st_transform(sf::st_crs(pop_density)),
    fun = mean, na.rm = TRUE
  )

  colnames(glonaf_pop_density)[2:6] =
    paste0("pop_density", seq(2000, 2020, by = 5))

  unified_glonaf_regions %>%
    as.data.frame() %>%
    select(OBJIDsic, IDregion) %>%
    cbind(glonaf_pop_density[, 2:6])
}

combine_species_socioecovars = function(
    glonaf_species_regions, glonaf_road_density, glonaf_pop_density,
    glonaf_pop_count, glonaf_gdp_research
) {

  list(
    glonaf_species_regions, glonaf_road_density, glonaf_pop_density,
    glonaf_pop_count, glonaf_gdp_research
  ) %>%
    # Repeatedly join datasets
    Reduce(
      function(x, y) full_join(x, select(y, -IDregion), by = "OBJIDsic"), .
    ) %>%
    filter(!is.na(species)) %>%  # Remove regions with no referenced species
    select(-OBJIDsic) %>%
    group_by(species) %>%
    # Compute average Socioecovars across full range of species
    summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
    # Compute a single pop. density and pop. count across range of species
    rowwise() %>%
    mutate(
      pop_density = mean(
        c_across(pop_density2000:pop_density2020), na.rm = TRUE
      ),
      pop_count   = mean(c_across(pop_count2000:pop_count2020), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    # Remove individual pop. count & pop. density columns
    select(-matches("[0-9]+"))

}
