get_socioeco_variables = function() {
  WDI::WDI(
    indicator = c(
      research_expenditure_perc_gdp = "GB.XPD.RSDV.GD.ZS",
      gdp_total                     = "6.0.GDP_current",
      gdp_per_capita                = "6.0.GDPpc_constant"
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
