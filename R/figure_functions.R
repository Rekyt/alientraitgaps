plot_euler_diagram_glonaf_species_in_databases = function(
  match_glonaf_tnrs, harmonized_try_glonaf, try_open_species,
  glonaf_bien_traits_count
) {
  glonaf_species = match_glonaf_tnrs %>%
    distinct(Accepted_species) %>%
    pull(Accepted_species)

  try_full_species = harmonized_try_glonaf %>%
    pull(species_accepted_try) %>%
    unique()

  try_open_species = try_open_species %>%
    pull(species_accepted_try) %>%
    unique()

  bien_species = glonaf_bien_traits_count %>%
    pull(scrubbed_species_binomial) %>%
    unique()


  full_list = list(
    glonaf   = glonaf_species,
    try_full = try_full_species,
    try_open = try_open_species,
    bien     = bien_species
  )

  euler_diagram = eulerr::euler(
    full_list
  )
}

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

plot_glonaf_trait_combination_frequency = function(try_trait_combination,
                                                   title = "") {
  try_trait_combination %>%
    ggplot(aes(x = trait_names)) +
    geom_bar() +
    geom_text(stat = 'count', aes(label = after_stat(count)), vjust = 1,
              size = rel(3), color = "white", face = "bold") +
    ggupset::scale_x_upset(n_intersections = 15) +
    labs(title = title)
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

plot_species_trait_combinations = function(numbers_trait_combinations,
                                           n_rank = 1e3) {
  numbers_trait_combinations %>%
    group_by(trait_db, trait_number) %>%
    arrange(desc(n_species)) %>%
    mutate(n_group = row_number()) %>%
    ungroup() %>%
    filter(n_group <= n_rank) %>%
    ggplot(aes(n_group, n_species, color = as.factor(trait_number))) +
    geom_line(size = 1) +
    geom_hline(yintercept = 1.5e4, linetype = 2, color = "darkred", size = 1) +
    facet_grid(
      cols = vars(trait_db),
      labeller = labeller(trait_db = c(bien     = "BIEN",
                                       try_open = "TRY (open)"))
    ) +
    scale_x_continuous(name = "Rank of combination of traits") +
    scale_y_log10(name = "Number of species") +
    scale_color_viridis_d(name = "Number of Traits") +
    labs(
      title = "Number of species per combinations of traits in both database",
      caption = paste0("Showing the ", n_rank, " most numerous combinations;\n",
                       "Dashed line indicates total species in GloNAF")
    ) +
    theme_bw() +
    theme(aspect.ratio = 1,
          legend.position = "top")
}
