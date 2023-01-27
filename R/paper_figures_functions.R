# Script with functions needed to generate final figures
# Main Figures -----------------------------------------------------------------

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

  patchwork::wrap_plots(fig_treemap_general, fig_trait_combination_taxonomy,
                        guides = "collect", ncol = 1) +
    patchwork::plot_annotation(tag_levels = "A") &
    theme(legend.position = "right", legend.direction = "vertical")

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


# Supplementary Figures --------------------------------------------------------

plot_proportion_species_with_trait = function(
    combined_traits, match_glonaf_tnrs
) {

  trait_number = combined_traits %>%
    group_by(species) %>%
    summarise(n_traits = n()) %>%
    group_by(n_traits) %>%
    summarise(n_species = n()) %>%
    arrange(desc(n_traits)) %>%
    mutate(cumulative_species = cumsum(n_species))

  n_total = sum(unique(match_glonaf_tnrs[["Accepted_species"]]) != "")

  rm(combined_traits, match_glonaf_tnrs)

  trait_number %>%
    ggplot(aes(n_traits, cumulative_species)) +
    geom_line(color = "darkblue") +
    scale_x_log10("Number of traits") +
    scale_y_continuous(
      "Cumulative number of species having at least X trait(s)",
      sec.axis = sec_axis(
        ~ ./n_total, name = "Cumulative proportion of species",
        labels = scales::label_percent()
      )
    ) +
    theme_bw() +
    theme(aspect.ratio = 1)

}


assemble_maps_number_of_traits = function(
  fig_map_median_n_traits_region, fig_map_sd_n_traits_region
) {
  patchwork::wrap_plots(
    fig_map_median_n_traits_region,
    fig_map_sd_n_traits_region,
    nrow = 2, ncol = 1, tag_level = "new"
  ) +
    patchwork::plot_annotation(tag_levels = "A")
}
