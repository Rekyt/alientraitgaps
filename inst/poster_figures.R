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

