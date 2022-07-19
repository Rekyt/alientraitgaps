assemble_fig1 = function(
    fig_combined_trait_heatmap, fig_species_per_trait_combined
) {
  patchwork::wrap_plots(
    fig_combined_trait_heatmap, fig_species_per_trait_combined,
    tag_level = "new", nrow = 1
  ) +
    patchwork::plot_annotation(tag_levels = "A")
}

assemble_fig3 = function(fig_map_alien_richness, fig_map_prop_trait_regions) {
  patchwork::wrap_plots(
    fig_map_alien_richness, fig_map_prop_trait_regions,
    tag_level = "new", ncol = 1
  ) +
    patchwork::plot_annotation(tag_levels = "A")
}
