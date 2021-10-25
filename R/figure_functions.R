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


plot_trait_ranks_multi_db = function(glonaf_trait_ranks) {
  glonaf_trait_ranks %>%
    filter(trait_number > 0) %>%
    mutate(trait_db = factor(trait_db,
                             levels = c("bien", "try_full", "try_extract"))
           ) %>%
    ggplot(aes(species_trait_number_rank, trait_number, color = trait_db)) +
    geom_point(size = 2/3) +
    scale_y_log10(name = "Number of traits") +
    scale_color_brewer(
      name = "Trait Database", palette = "Set2",
      guide = guide_legend(override.aes = list(size = 2)),
      labels = c(bien = "BIEN",
                 try_full = "TRY (full)",
                 try_extract = "TRY (extract)")
    ) +
    labs(x = "Rank of species",
         title = "Number of trait per GloNAF species across databases") +
    theme_bw() +
    theme(aspect.ratio = 1)
}
