plot_trait_number_try_glonaf_species = function(try_number_trait) {
  try_number_trait %>%
    ggplot(aes(trait_number)) +
    geom_histogram(color = "white") +
    scale_x_log10(name = "Number of ≠ traits in TRY") +
    scale_y_continuous(name = "Number of species") +
    labs(title = "GloNAF species in TRY (~15k)",
         caption = "GloNAF species harmonized using TNRS") +
    theme_bw() +
    theme(aspect.ratio = 1,
          panel.grid = element_blank())
}

plot_number_species_per_try_trait = function(try_number_species_per_trait) {
  try_number_species_per_trait %>%
    slice_max(n_sp, n = 15) %>%
    ggplot(aes(n_sp, TraitName)) +
    geom_point() +
    scale_x_log10(name = "Number of species measured") +
    scale_y_discrete(labels = scales::wrap_format(25)) +
    labs(y = "Trait name",
         title = "15 Most frequent trait in TRY from GloNAF species (15k)",
         caption = "TRY open data on a selected subset of traits") +
    theme_bw() +
    theme(aspect.ratio = 1)
}

plot_glonaf_try_trait_combination_frequency = function(try_trait_combination) {
  try_trait_combination %>%
    ggplot(aes(x = trait_names)) +
    geom_bar() +
    geom_text(stat='count', aes(label = after_stat(count)), vjust = -1) +
    ggupset::scale_x_upset(n_intersections = 8) +
    labs(caption = "TRY open data on a selected subset of traits",
         subtitle = "Most commonly measured combination of traits on aliens")
}
