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

  unified_glonaf_regions = unified_glonaf_regions %>%
    sf::st_transform(
      sf::st_crs("+proj=eqearth")
    )

  # Combine WDI with Unified
  unified_glonaf_regions %>%
    sf::st_join(
      world_sf %>%
        sf::st_transform(sf::st_crs(unified_glonaf_regions))
    ) %>%
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

get_gift_socioecovars = function(gift_api, gift_version) {

  GIFT::GIFT_env(
    miscellaneous = c("area", "biome"),
    rasterlayer = c("hf_v2geo", "hii_v2geo", "GDP_PPP_2015",
                    "access_cities_2015"),
    GIFT_version = gift_version,
    api = gift_api
  )

}


count_socioecovars_cover = function(
    gift_socioecovars, gift_unified_distribution
) {

  # Transform socioecovars appropriately
  gift_trans = gift_socioecovars %>%
    select(-biome) %>%
    mutate(
      across(mean_hf_v2geo:mean_access_cities_2015, ~!is.na(.x)),
      area = as.numeric(area)
    )

  # Count proportion of range covered
  gift_unified_distribution %>%
    select(entity_ID, Accepted_species, status) %>%
    left_join(gift_trans, by = "entity_ID") %>%
    group_by(Accepted_species) %>%
    summarise(
      # Count number of regions/area of total range
      n_total = n(),
      area_total = sum(area, na.rm = TRUE),
      # Number of regions/area of range with given described variables
      across(
        mean_hf_v2geo:mean_access_cities_2015,
        .fns = list(
          n_described = ~sum(.x, na.rm = TRUE),
          area_described = ~sum(area[isTRUE(.x)], na.rm = TRUE)
        )
      ),
      # Minimum range described across all variables
      min_n_described = min(
        c_across(ends_with("n_described")), na.rm = TRUE
      ),
      min_area_described = min(
        c_across(ends_with("area_described")), na.rm = TRUE
      ),
      # Proportion of range described
      prop_n_described = min_n_described/n_total,
      prop_area_described = min_area_described/area_total
    )

}

compute_gift_species_socioecovars = function(
    gift_socioecovars, gift_unified_distribution
) {

  gift_unified_distribution %>%
    select(entity_ID, Accepted_species, status) %>%
    full_join(gift_socioecovars, by = "entity_ID") %>%
    group_by(Accepted_species) %>%
    summarise(
      across(
        mean_hf_v2geo:mean_access_cities_2015, .fns = ~ mean(.x, na.rm = TRUE)
      ),
      area          = sum(as.numeric(area)),
      n_total       = n(),
      n_native      = sum(status == "native"),
      n_naturalized = sum(status == "naturalized"),
      n_non_native  = sum(status == "non-native"),
      n_unknown     = sum(status == "unknown")
    )

}

combine_and_filter_socioecovars = function(
    species_socioecovars, species_gift_count_socioecovars,
    species_gift_socioecovars
) {

  species_socioecovars %>%
    distinct(species) %>%
    inner_join(
      species_gift_count_socioecovars %>%
        filter(prop_n_described >= 0.8) %>%
        distinct(species = Accepted_species),
    by = "species"
    ) %>%
    left_join(
      species_gift_socioecovars,
      by = c(species = "Accepted_species")
    )

}
