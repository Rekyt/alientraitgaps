# Script with functions needed to generate final figures
# Main Figures -----------------------------------------------------------------

assemble_fig1 = function(
    fig_combined_trait_heatmap, fig_species_per_trait_combined
) {
  patchwork::wrap_plots(
    fig_combined_trait_heatmap, fig_species_per_trait_combined,
    tag_level = "new", nrow = 2, ncol = 1
  ) +
    patchwork::plot_annotation(tag_levels = "A")
}

assemble_fig2 = function(fig_treemap_general, fig_trait_combination_taxonomy) {

  patchwork::wrap_plots(fig_treemap_general, fig_trait_combination_taxonomy,
                        guides = "collect", ncol = 1) +
    patchwork::plot_annotation(tag_levels = "A") &
    theme(legend.position = "right", legend.direction = "vertical",
          legend.spacing.y = unit(0.5, "lines")) &
    # Allows to space legend vertically
    guides(fill = guide_legend(byrow = TRUE))

}

assemble_fig3 = function(fig_map_alien_richness, fig_map_prop_trait_regions) {
  patchwork::wrap_plots(
    fig_map_alien_richness, fig_map_prop_trait_regions,
    tag_level = "new", ncol = 1, heights = c(1/3, 2/3)
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


create_fig5 = function(trait_knowledge_model) {

  # Get model parameters
  mp = parameters::model_parameters(
    trait_knowledge_model,
    drop = "growth_form"
  )

  # Tidy up name of parameters
  mp = mp |>
    mutate(Parameter = case_match(
      Parameter,
      "total_range_size"              ~ "Total Range Size",
      "avg_human_influence_index"     ~ "Avg. Human Influence Index",
      "n_biomes"                      ~ "Number of Biomes",
      "sd_human_influence_index"      ~ "Std. Dev.\nHuman Influence Index",
      "avg_accessibility"             ~ "Accessibility",
      "avg_gdp_over_non_native_range" ~ "Avg. GDP\n(non-native range)",
      "non_native_range_size"         ~ "Non-native Range Size",
      "avg_gdp_over_native_range"     ~ "Avg. GDP\n(native range)"
    ))

  # Remove object laying around to avoid having huge ggplot2 objects
  rm(trait_knowledge_model)

  plot(
    mp, show_labels = TRUE, size_point = 0.6, size_text = 3.5,
    sort = "descending"
  ) +
    ggplot2::theme_bw() +
    ggplot2::guides(color = "none")

}


# Supplementary Figures --------------------------------------------------------

plot_partial_residuals = function(trait_knowledge_model) {

  sjPlot::set_theme(base = ggplot2::theme_bw())

  all_plots = sjPlot::plot_model(
    trait_knowledge_model, type = "pred", title = "", case = "title"
  )

  patchwork::wrap_plots(all_plots) &
    ggplot2::theme(
      axis.title = element_text(color = "black"),
      axis.text  = element_text(color = "black")
    )

}

plot_proportion_species_with_trait = function(
    combined_traits, glonaf_tnrs
) {

  trait_number = combined_traits[[1]] %>%
    filter(in_glonaf) |>
    mutate(n_traits = purrr::map_int(traits, length)) %>%
    group_by(n_traits) %>%
    summarise(n_species = n()) %>%
    arrange(desc(n_traits)) %>%
    mutate(cumulative_species = cumsum(n_species))

  n_total = sum(unique(glonaf_tnrs[["Accepted_species"]]) != "")

  rm(combined_traits, glonaf_tnrs)

  trait_number %>%
    ggplot(aes(n_traits, cumulative_species)) +
    geom_line(color = "darkblue", linewidth = 1) +
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


plot_treemaps_with_number_of_traits = function(
  glonaf_family, trait_combinations_full
) {

  tax_comb = glonaf_family %>%
    distinct(species = taxa_accepted, family = family_wcvp) %>%
    inner_join(trait_combinations_full, by = "species") %>%
    mutate(n_traits = purrr::map_int(traits, length)) %>%
    mutate(across(where(is.character), ~iconv(.x, "latin1", to = "UTF-8"))) %>%
    select(species, family, n_traits) %>%
    mutate(
      trait_category = case_when(
        n_traits <= 10   ~ "1 - 10",
        n_traits <= 100  ~ "10 - 100",
        n_traits <= 1000 ~ "100 - 1000"
      )
    ) %>%
    count(family, trait_category, name = "n_species")

  # Clean environment
  rm(glonaf_family, trait_combinations_full)

  # Only by trait category
  general_treemap = tax_comb %>%
    group_by(trait_category) %>%
    summarise(n_species = sum(n_species)) %>%
    mutate(proportion = n_species/sum(n_species)) %>%
    ggplot(
      aes(
        area = n_species, fill = trait_category,
        label = paste0(
          "n = ", format(n_species, big.mark = ",", trim = TRUE),
          "\n(", round(proportion * 100, 0), "%)"
        )
      )
    ) +
    treemapify::geom_treemap(color = NA) +
    treemapify::geom_treemap_text(
      aes(color = trait_category), place = "centre", show.legend = FALSE
    ) +
    scale_color_manual(
      values = c("1 - 10"     = "white",
                 "10 - 100"   = "white",
                 "100 - 1000" = "black")
    ) +
    scale_fill_manual(
      "Number\nof Known Traits",
      values = c("1 - 10"     = "#440154FF",
                 "10 - 100"   = "#21908CFF",
                 "100 - 1000" = "#FDE725FF")
    ) +
    theme(
      aspect.ratio = 1,
      legend.position = "top"
    )

  # By family by trait category
  family_treemap = tax_comb %>%
    mutate(family = ifelse(family == "", "Other", family)) %>%
    ggplot(
      aes(area = n_species, label = n_species, fill = trait_category,
          subgroup = family)
    ) +
    treemapify::geom_treemap(color = NA) +
    treemapify::geom_treemap_text(
      aes(colour = trait_category), size = 7.5, show.legend = FALSE
    ) +
    treemapify::geom_treemap_subgroup_border(
      size = 0.5, color = "white", show.legend = FALSE
    ) +
    treemapify::geom_treemap_subgroup_text(
      place = "centre", grow = TRUE, alpha = 4/5, colour = "white",
      fontface = "bold.italic", min.size = 6.5, show.legend = FALSE
    ) +
    scale_color_manual(
      values = c("1 - 10"     = "white",
                 "10 - 100"   = "white",
                 "100 - 1000" = "black")
    ) +
    scale_fill_manual(
      "Number\nof Known Traits",
      values = c("1 - 10"     = "#440154FF",
                 "10 - 100"   = "#21908CFF",
                 "100 - 1000" = "#FDE725FF")
    ) +
    theme(
      aspect.ratio = 1,
      legend.position = "top"
    )

  patchwork::wrap_plots(
    general_treemap, family_treemap, nrow = 2, ncol = 1, guides = "collect"
  ) +
    patchwork::plot_annotation(tag_levels = "A") &
    theme(legend.position = "right")

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

plot_all_models = function(all_models) {

  sjPlot::plot_models(
    all_models,
    title = "",
    axis.labels = c(
      growth_formtree               = "Growth Form: Tree",
      growth_formshrub              = "Growth Form: Shrub",
      growth_formherb               = "Growth Form: Herb",
      growth_formother              = "Growth Form: Other",
      total_range_size              = "Total Range Size",
      avg_human_influence_index     = "Avg. Human Influence Index",
      n_biomes                      = "Number of Biomes",
      sd_human_influence_index      = "Std. Dev.\nHuman Influence Index",
      avg_accessibility             = "Accessibility",
      avg_gdp_over_non_native_range = "Avg. GDP\n(non-native range)",
      non_native_range_size         = "Non-native Range Size",
      avg_gdp_over_native_range     = "Avg. GDP\n(native range)"
    ),
    wrap.labels = 40,
    axis.title = "",
    m.labels = c("70% var. coverage", "80% var. coverage", "90% var. coverage"),
    legend.title = "Models",
    show.p = FALSE,
    value.size = 2,
    rm.terms = c("growth_formtree", "growth_formshrub", "growth_formherb",
                 "growth_formother")
  ) +
    theme_bw() +
    theme(legend.position = "top")

}
