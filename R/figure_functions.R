plot_trait_number_try_glonaf_species = function(try_number_trait) {
  try_number_trait %>%
    ggplot(aes(trait_number)) +
    geom_histogram(color = "white") +
    scale_x_log10(name = "Number of traits in TRY") +
    scale_y_continuous(name = "Number of species") +
    labs(title = "Traits of GloNAF species in TRY (~15k)",
         caption = "GloNAF species harmonized using TNRS; All TRY open data") +
    theme_bw() +
    theme(aspect.ratio = 1,
          panel.grid = element_blank())
}

plot_number_species_per_try_trait = function(
  try_number_species_per_trait,
  bien_number_species_per_trait) {
  list(
    try_open = try_number_species_per_trait %>%
      rename(trait_name = TraitName),
    bien     = bien_number_species_per_trait
  ) %>%
    bind_rows(.id = "trait_db") %>%
    group_by(trait_db) %>%
    slice_max(n_sp, n = 15) %>%
    ungroup() %>%
    mutate(trait_name = forcats::fct_reorder(factor(trait_name), n_sp)) %>%
    ggplot(aes(n_sp, trait_name)) +
    geom_point() +
    facet_wrap(
      vars(trait_db), scales = "free_y",
      labeller = labeller(
        trait_db = as_labeller(c(bien = "BIEN", try_open = "TRY (open)"))
      )
    ) +
    scale_x_log10(name = "Number of species measured") +
    scale_y_discrete(labels = scales::wrap_format(25)) +
    labs(y = "Trait name",
         title = "15 most frequently measured trait",
         caption = "All TRY open data; All BIEN trait data") +
    theme_bw() +
    theme(aspect.ratio = 1)
}

plot_glonaf_try_trait_combination_frequency = function(try_trait_combination) {
  try_trait_combination %>%
    ggplot(aes(x = trait_names)) +
    geom_bar() +
    geom_text(stat = 'count', aes(label = after_stat(count)), vjust = 1,
              size = rel(3), color = "white", face = "bold") +
    ggupset::scale_x_upset(n_intersections = 15) +
    labs(caption = "All TRY open data",
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
                 try_full = "TRY (all data)",
                 try_extract = "TRY (open data)")
    ) +
    labs(x = "Rank of species (out of 15k GloNAF species)",
         title = "Number of trait per GloNAF species across databases") +
    theme_bw() +
    theme(aspect.ratio = 1)
}
