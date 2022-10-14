assemble_fig1 = function(
    fig_combined_trait_heatmap, fig_species_per_trait_combined
) {
  patchwork::wrap_plots(
    fig_combined_trait_heatmap, fig_species_per_trait_combined,
    tag_level = "new", nrow = 1, widths = c(2/5, 3/5)
  ) +
    patchwork::plot_annotation(tag_levels = "A")
}

assemble_fig2 = function(fig_treemap_general, fig_trait_combination_taxonomy) {

  patchwork::guide_area() /
    (
      (fig_treemap_general + theme(legend.position = "none"))
      + fig_trait_combination_taxonomy
    ) +
    patchwork::plot_annotation(tag_levels = "A") +
    patchwork::plot_layout(guides = 'collect', heights = c(1/10, 9/10))
}

assemble_fig3 = function(fig_map_alien_richness, fig_map_prop_trait_regions) {
  patchwork::wrap_plots(
    fig_map_alien_richness, fig_map_prop_trait_regions,
    tag_level = "new", ncol = 1
  ) +
    patchwork::plot_annotation(tag_levels = "A")
}

assemble_fig4 = function(
  fig_status_prop_comb, fig_widest_range_trait_comb_prop
) {
  patchwork::wrap_plots(
    fig_status_prop_comb, fig_widest_range_trait_comb_prop,
    tag_level = "new", nrow = 1
  ) +
    patchwork::plot_annotation(tag_levels = "A")
}
