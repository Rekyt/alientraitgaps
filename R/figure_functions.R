plot_euler_diagram_glonaf_species_in_databases = function(
  match_glonaf_tnrs, harmonized_try_glonaf, try_open_species,
  glonaf_bien_traits_count, match_austraits_tnrs
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

  austraits_species = match_austraits_tnrs %>%
    distinct(Accepted_species) %>%
    pull(Accepted_species)

  full_list = list(
    glonaf    = glonaf_species,
    try_full  = try_full_species,
    try_open  = try_open_species,
    bien      = bien_species,
    austraits = austraits_species
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
        trait_db = c(bien = "BIEN", try_open = "TRY (open)",
                     aus_traits = "AusTraits")
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

plot_number_species_per_trait_combined = function(combined_traits) {
  combined_traits %>%
    count(consolidated_name, sort = TRUE, name = "n_species") %>%
    slice_max(n_species, n = 20) %>%
    ggplot(aes(n_species, forcats::fct_reorder(consolidated_name, n_species))) +
    geom_point() +
    geom_vline(xintercept = 16528, linetype = 2, color = "darkred", size = 1) +
    scale_x_continuous(
      name = "Number of species with traits",
      sec.axis = sec_axis(trans = ~.x/16528, labels = scales::percent_format())
    ) +
    scale_y_discrete(name = "Trait name", labels = label_wrap_gen(30)) +
    labs(title = "15 most frequently measured trait") +
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
      labeller = labeller(trait_db = c(bien      = "BIEN",
                                       try_open  = "TRY (open)",
                                       aus_traits = "AusTraits"))
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

plot_combined_traits_heatmap = function(combined_traits) {

  # Get combinations of trait measured by species ordered by frequency
  combined_traits %>%
    mutate(
      species_fact = factor(species) %>%
        forcats::fct_infreq(),
      trait_fact  = factor(consolidated_name) %>%
        forcats::fct_infreq(),
      species_rank = as.numeric(species_fact),
      trait_rank = as.numeric(trait_fact),
      value = TRUE
    ) %>%
    select(species_rank, trait_rank, value) %>%
    tidyr::complete(species_rank, trait_rank,
                    fill = list(value = FALSE)) %>%
    # Plot as a heatmap
    ggplot(aes(trait_rank, species_rank, fill = value)) +
    geom_raster() +
    labs(x    = "Trait Rank (most to least measured)",
         y    = "Species Rank (most to least measured)",
         fill = "Does species has this trait?",
         caption = paste0("Using consolidated trait data from AusTraits, ",
                          "BIEN, and TRY")) +
    scale_fill_viridis_d() +
    theme_bw() +
    theme(aspect.ratio = 1, legend.position = "top")
}

plot_number_specific_trait_combination = function(contain_trait_combination) {
  contain_trait_combination %>%
    select(-traits) %>%
    tidyr::pivot_longer(!species, names_to = "comb_name",
                        values_to = "comb_value") %>%
    filter(comb_value) %>%
    count(comb_name, sort = TRUE, name = "n_species") %>%
    filter(comb_name != "in_glonaf") %>%
    ggplot(
      aes(
        n_species,
        forcats::fct_relevel(comb_name, "has_bergmann", "has_diaz", "has_lhs",
                             "has_at_least_one_trait", "in_glonaf")
      )
    ) +
    geom_vline(xintercept = 16538, linetype = 2, size = 1, color = "darkred") +
    geom_point(size = 2, color = "darkblue") +
    scale_x_continuous(
      sec.axis = sec_axis(~./16538, labels = scales::percent_format())
    ) +
    scale_y_discrete(
      labels = c(in_glonaf = "In GloNAF",
                 has_at_least_one_trait = "At least one trait",
                 has_lhs   = "Leaf-Height-Seed\n(Westoby 1998)",
                 has_diaz  = "Aboveground traits\n(Díaz et al., 2016)",
                 has_bergmann = "Root traits\n(Bergmann et al., 2020)")
    ) +
    labs(x = "Number of species",
         y = "Trait Combination") +
    theme_bw() +
    theme(aspect.ratio = 1)
}

plot_taxonomy_treemap_trait_combination = function(
  combined_traits_taxonomy, contain_trait_combination
) {
  combined_traits_taxonomy %>%
    mutate(species = ifelse(is.na(species), paste(genus, epithet), species)) %>%
    distinct(species, genus, family) %>%
    right_join(
      contain_trait_combination %>%
        select(-traits),
      by = "species") %>%
    filter(!is.na(genus)) %>%
    ggplot(
      aes(area = 1,
          fill = interaction(in_glonaf, has_at_least_one_trait, has_lhs,
                             has_diaz),
          label = genus, subgroup = family)
    ) +
    treemapify::geom_treemap(color = NA) +
    treemapify::geom_treemap_subgroup_border(size = 0.7, color = "white") +
    treemapify::geom_treemap_subgroup_text(
      place = "centre", grow = TRUE, alpha = 0.7, colour = "black",
      fontface = "italic", min.size = 0) +
    scale_fill_manual(
      name = "Trait combination",
      labels = c(
        "TRUE.FALSE.FALSE.FALSE" = "No trait",
        "TRUE.TRUE.FALSE.FALSE"  = "At least\none trait",
        "TRUE.TRUE.TRUE.FALSE"   = "Leaf-Height-Seed\n(Westoby 1998)",
        "TRUE.TRUE.TRUE.TRUE"    = "Aboveground traits\n(Díaz et al., 2016)"
      ),
      values = c(
        "TRUE.FALSE.FALSE.FALSE" = "lightgray",
        "TRUE.TRUE.FALSE.FALSE"  = "#b2df8a",
        "TRUE.TRUE.TRUE.FALSE"   = "#a6cee3",
        "TRUE.TRUE.TRUE.TRUE"    = "#1f78b4"
      )
    ) +
    theme(legend.position = "top")
}

plot_taxonomy_treemap_number_traits = function(
  combined_traits_taxonomy, contain_trait_combination, logged = TRUE
) {

  if (logged) {
    color_scale = scale_fill_viridis_c(
      name = "Number of traits",
      trans = scales::pseudo_log_trans(base = 10),
      breaks = c(1, 10, 100, 1000)
    )
  } else {
    color_scale = scale_fill_viridis_c(
      name = "Number of traits"
    )
  }

  combined_traits_taxonomy %>%
    mutate(species = ifelse(is.na(species), paste(genus, epithet), species)) %>%
    distinct(species, genus, family) %>%
    right_join(contain_trait_combination, by = "species") %>%
    mutate(number_of_traits = purrr::map_int(traits, length)) %>%
    filter(!is.na(genus)) %>%
    ggplot(
      aes(area = 1, fill = number_of_traits + 1, label = genus,
          subgroup = family)
    ) +
    treemapify::geom_treemap(color = NA) +
    treemapify::geom_treemap_subgroup_border(size = 0.5, color = "white") +
    treemapify::geom_treemap_subgroup_text(
      place = "centre", grow = TRUE, alpha = 0.5, colour = "white",
      fontface = "italic", min.size = 0
    ) +
    color_scale +
    theme(legend.position = "top")
}
