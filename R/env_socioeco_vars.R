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
