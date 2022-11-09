library("ggplot2")
library("patchwork")

pfig1_trait_heatmap_and_freq = patchwork::wrap_plots(
  fig_combined_trait_heatmap, fig_species_per_trait_combined,
  tag_level = "new", nrow = 1, widths = c(1, 1.618)
) +
  patchwork::plot_annotation(tag_levels = "A")

ggsave(
  "../poster_alientraitgaps_sfe_metz_2022/fig1_heatmap_frequency.svg",
  plot = pfig1_trait_heatmap_and_freq,
  height = 618, width = 1000, units = "mm", scale = 1/4
)


# Maps

tar_load(
  c(regions_trait_prop, glonaf_small_islands, glonaf_mainland_large_islands)
)


# Background map
world_sf = rnaturalearth::ne_countries(returnclass = "sf") %>%
  sf::st_transform(crs = "+proj=eqearth")

# Pivot trait data to be usable across facets
pivoted_data = regions_trait_prop %>%
  select(OBJIDsic, prop_with_any_trait:has_bergmann_prop) %>%
  tidyr::pivot_longer(
    !OBJIDsic, names_to = "prop_name", values_to = "prop_value"
  ) %>%
  mutate(
    prop_name = factor(
      prop_name,
      levels = c("prop_with_any_trait", "has_lhs_prop", "has_diaz_prop",
                 "has_bergmann_prop")
    )
  )

# Mainland regions
mainland_pivot = glonaf_mainland_large_islands %>%
  inner_join(pivoted_data, by = "OBJIDsic")

# Island regions
island_pivot = glonaf_small_islands %>%
  inner_join(pivoted_data, by = "OBJIDsic")

# Clean environment
rm(pivoted_data, glonaf_mainland_large_islands, glonaf_small_islands,
   regions_trait_prop)

# Actual plot
actual_map = mainland_pivot %>%
  ggplot(aes(fill = prop_value)) +
  geom_sf(data = world_sf, fill = "gray85", color = "gray65", size = 1/100) +
  # Non-small islands and mainlands
  geom_sf(color = NA, size = 1/100) +
  # Small islands
  geom_sf(
    aes(color = prop_value),
    fill = NA,
    data = island_pivot,
    size = 1.2, shape = 21, stroke = 0.4
  ) +
  facet_wrap(
    vars(prop_name),
    labeller = labeller(
      prop_name = c(
        has_bergmann_prop = "Root Traits",
        has_diaz_prop     = "Aboveground Traits",
        has_lhs_prop      = "LHS Traits",
        prop_with_any_trait = "Any trait"
      )
    )
  ) +
  scale_fill_viridis_b(
    name = "Proportion of alien species\nwith trait combination",
    labels = scales::percent_format(), n.breaks = 6, show.limits = TRUE
  ) +
  scale_color_viridis_b(
    name = "Proportion of alien species\nwith trait combination",
    labels = scales::percent_format(), n.breaks = 6, show.limits = TRUE
  ) +
  ylim(-5747986, NA) +  # Remove whatever is below 60°S
  theme_void() +
  theme(
    legend.position = "top",
    legend.key.width = unit(2, "lines"),
    strip.background = element_blank(),
    plot.margin      = margin(b = 3/11)
  )

ggsave(
  "../poster_alientraitgaps_sfe_metz_2022/fig_map_alien_richness.svg",
  tar_read(fig_map_alien_richness),
  height = 618, width = 1000, units = "mm", scale = 1/4
)

ggsave(
  "../poster_alientraitgaps_sfe_metz_2022/fig_map_prop_trait_regions.png",
  tar_read(fig_map_prop_trait_regions),
  height = 618, width = 1000, units = "mm", scale = 1/4, dpi = 500
)


# Figure 2/3 proportion of combination versus status and widenspreadness

pfig3_trait_comb_prop_status_regions = patchwork::wrap_plots(
  targets::tar_read(fig_status_prop_comb),
  targets::tar_read(fig_widest_range_trait_comb_prop),
  tag_level = "new", ncol = 1
) &
  theme_bw(base_size = 25) &
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

ggsave(
  "../poster_alientraitgaps_sfe_metz_2022/fig_status_widespread_trait_prop.svg",
  pfig3_trait_comb_prop_status_regions,
  width = 315, height = 260, units = "mm"
)
