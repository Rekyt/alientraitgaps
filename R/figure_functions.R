# Functions to plot figures
# Trait Counts -----------------------------------------------------------------
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

plot_number_species_per_trait_combined = function(combined_traits) {

  max_20_traits = combined_traits %>%
    count(consolidated_name, sort = TRUE, name = "n_species") %>%
    filter(!grepl("Elevational", consolidated_name, fixed = TRUE)) %>%
    slice_max(n_species, n = 20)

  total_sp = combined_traits %>%
    pull(species) %>%
    unique() %>%
    length()

  max_20_traits %>%
    ggplot(aes(n_species, forcats::fct_reorder(consolidated_name, n_species))) +
    geom_point() +
    geom_text(
      aes(label = paste0(round((n_species/total_sp) * 100, 0), "%")),
      hjust = -0.4, size = 2.6, vjust = 0
    ) +
    # 50% vertical line
    geom_text(
      label = "50%", color = "darkblue", x = total_sp/2, y = 20, hjust = -0.4,
      vjust = 0.5, size = 2.6
    ) +
    geom_vline(
      xintercept = total_sp/2, linetype = 2, color = "darkblue", size = 1
    ) +
    # 100% vertical line
    geom_vline(
      xintercept = total_sp, linetype = 2, color = "darkred", size = 1
    ) +
    scale_x_continuous(
      name = "Number of species with traits",
      sec.axis = sec_axis(
        trans = ~.x/total_sp, labels = scales::percent_format()
      )
    ) +
    scale_y_discrete(name = "Trait name", labels = label_wrap_gen(30)) +
    labs(title = "20 most frequently measured traits") +
    theme_bw()
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

plot_number_trait_categories_per_invasion_status = function(
    glonaf_status_trait_cat
) {

  status_count = glonaf_status_trait_cat %>%
    count(status_name) %>%
    mutate(
      new_name = ifelse(status_name == "aliens", "unspecified", status_name),
      label = paste0(tools::toTitleCase(status_name), "\n(n=", n, ")")
    ) %>%
    select(status_name, label) %>%
    tibble::deframe()

  glonaf_status_trait_cat %>%
    select(-c(has_at_least_one_trait:has_bergmann)) %>%
    tidyr::pivot_longer(
      leaf:root, names_to = "cat_name", values_to = "cat_value"
    ) %>%
    mutate(
      status_name = factor(
        status_name, level = c("alien", "naturalized", "invasive")
      )
    ) %>%
    ggplot(aes(cat_value, status_name)) +
    ggridges::geom_density_ridges(
      scale = 0.95, quantile_lines = TRUE, quantiles = 4, calc_ecdf = TRUE
    ) +
    facet_wrap(
      vars(cat_name), scales = "free_x",
      labeller = labeller(
        cat_name = function(x) snakecase::to_any_case(x, case = "title")
      )
    ) +
    scale_y_discrete("Invasion Status", labels = status_count) +
    scale_fill_viridis_d(name = "Quartiles") +
    theme_bw() +
    theme(
      strip.background = element_blank(),
      axis.text.y = element_text(vjust = 0)
    )
}

plot_trait_comb_proportion_per_invasion_status = function(
    glonaf_status_trait_cat
) {
  # Axis label with number of species per invasion status
  status_count = glonaf_status_trait_cat %>%
    count(status_name) %>%
    mutate(
      new_name = ifelse(status_name == "aliens", "unspecified", status_name),
      label = paste0(tools::toTitleCase(status_name), "\n(n=", n, ")")
    ) %>%
    select(status_name, label) %>%
    tibble::deframe()

  glonaf_status_trait_cat %>%
    select(status_name, species, has_at_least_one_trait:has_bergmann) %>%
    tidyr::pivot_longer(
      has_at_least_one_trait:has_bergmann, names_to = "comb_name",
      values_to = "comb_value"
    ) %>%
    mutate(
      status_name = factor(
        status_name, level = c("alien", "naturalized", "invasive")
      ),
      comb_name = factor(
        comb_name,
        levels = c("has_at_least_one_trait", "has_lhs", "has_diaz",
                   "has_bergmann")
      )
    ) %>%
    ggplot(aes(y = status_name, fill = comb_value)) +
    geom_bar(position = "fill", width = 2/3) +
    facet_wrap(
      vars(comb_name), scales = "free_x",
      labeller = labeller(
        comb_name = c(
          has_bergmann     = "Root Traits\n(4 traits, Bergmann et al. 2020)",
          has_diaz         = "Aboveground traits\n(6 traits, Díaz et al. 2016)",
          has_lhs          = "Leaf-Height-Seed mass\n(3 traits, Westoby 2002)",
          has_at_least_one_trait = "At least one trait"
        )
      )
    ) +
    scale_x_continuous(
      "Proportion of species", labels = scales::label_percent()
    ) +
    scale_y_discrete("Invasion Status", labels = status_count) +
    scale_fill_brewer(
      "Known Trait Combination?", palette = "Set1",
      labels = c(`FALSE` = "No", `TRUE` = "Yes")
    ) +
    theme_bw() +
    theme(
      strip.background = element_blank(),
      legend.position = "top"
    )
}


plot_number_trait_categories_per_range_size = function(
    glonaf_most_distributed_species, species_trait_categories
) {

  list(
    top = glonaf_most_distributed_species %>%
      select(-bootstrap_df) %>%
      tidyr::unnest(top_species),
    bootstrap = glonaf_most_distributed_species %>%
      select(-top_species) %>%
      tidyr::unnest(bootstrap_df)
  ) %>%
    bind_rows(.id = "boot_name") %>%
    inner_join(
      species_trait_categories, by = "species"
    ) %>%
    tidyr::pivot_longer(
      leaf:root, names_to = "cat_name", values_to = "cat_value"
    ) %>%
    ggplot(aes(cat_value, area_type, fill = boot_name)) +
    geom_violin(position = position_dodge(width = 0.75)) +
    facet_wrap(
      vars(cat_name), scales = "free_x",
      labeller = labeller(cat_name = snakecase::to_title_case)
    ) +
    labs(x = "Number of traits", y = NULL, fill = "Distribution type") +
    scale_fill_discrete(
      labels = c(top = "Top Species", bootstrap = "Bootstrap")
    ) +
    scale_y_discrete(
      labels = c(total_area = "Total Area", n_regions = "# Regions")
    ) +
    theme_bw() +
    theme(
      strip.background = element_blank(),
      legend.position = "top"
    )

}

plot_trait_combination_per_range_size = function(
    glonaf_most_distributed_species, contain_trait_combination
) {
  list(
    top = glonaf_most_distributed_species %>%
      select(-bootstrap_df) %>%
      tidyr::unnest(top_species),
    bootstrap = glonaf_most_distributed_species %>%
      select(-top_species) %>%
      tidyr::unnest(bootstrap_df)
  ) %>%
    bind_rows(.id = "boot_name") %>%
    select(-area_value) %>%
    inner_join(
      contain_trait_combination %>%
        select(-traits, -in_glonaf),
      by = "species"
    ) %>%
    tidyr::pivot_longer(
      has_at_least_one_trait:has_bergmann, names_to = "comb_name",
      values_to = "comb_value"
    ) %>%
    mutate(
      comb_name = factor(
        comb_name,
        levels = c("has_at_least_one_trait", "has_lhs", "has_diaz",
                   "has_bergmann")
      )
    ) %>%
    ggplot(aes(y = interaction(boot_name, area_type), fill = comb_value)) +
    geom_bar(position = "fill", width = 2/3) +
    facet_wrap(
      vars(comb_name),
      labeller = labeller(
        comb_name = c(
          has_bergmann     = "Root Traits\n(4 traits, Bergmann et al. 2020)",
          has_diaz         = "Aboveground traits\n(6 traits, Díaz et al. 2016)",
          has_lhs          = "Leaf-Height-Seed mass\n(3 traits, Westoby 2002)",
          has_at_least_one_trait = "At least one trait"
        )
      )
    ) +
    scale_x_continuous(
      "Proportion of Species", labels = scales::label_percent()
    ) +
    scale_y_discrete(
      NULL,
      labels = c("top.total_area" = "Area (Top)",
                 "bootstrap.total_area" = "Area (Bootstrap)",
                 "top.n_regions"  = "# Regions (Top)",
                 "bootstrap.n_regions" = "# Regions (Bootstrap)")
    ) +
    scale_fill_brewer(
      "Known Trait Combination?", palette = "Set1",
      labels = c(`FALSE` = "No", `TRUE` = "Yes")
    ) +
    theme_bw() +
    theme(legend.position = "top",
          strip.background = element_blank())
}


# Taxonomic Treemaps -----------------------------------------------------------

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
    mutate(across(where(is.character), ~iconv(.x, "latin1", to = "UTF-8"))) %>%
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
    mutate(across(where(is.character), ~iconv(.x, "latin1", to = "UTF-8"))) %>%
    ggplot(
      aes(area = 1, fill = number_of_traits + 1, label = genus,
          subgroup = family)
    ) +
    treemapify::geom_treemap(color = NA) +
    treemapify::geom_treemap_subgroup_border(size = 0.5, color = "white") +
    treemapify::geom_treemap_subgroup_text(
      place = "centre", grow = TRUE, alpha = 0.7, colour = "white",
      fontface = "italic", min.size = 0
    ) +
    color_scale +
    theme(legend.position = "top")
}


# Missing Traits ---------------------------------------------------------------

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
    labs(
      x    = "Trait Rank (most to least measured)",
      y    = "Species Rank (most to least measured)",
      fill = "Does species has this trait?"
    ) +
    scale_fill_viridis_d(labels = c(`TRUE` = "Yes", `FALSE` = "No")) +
    coord_cartesian(expand = FALSE) +
    theme_bw() +
    theme(aspect.ratio = 1, legend.position = "top")
}


plot_miss_trait_categories_per_species = function(species_trait_categories) {
  species_trait_categories %>%
    select(-species) %>%
    mutate(across(where(is.numeric), ~ifelse(.x == 0, NA_integer_, 1L))) %>%
    janitor::clean_names(case = "title") %>%
    visdat::vis_miss(cluster = TRUE, sort_miss = TRUE) +
    scale_fill_viridis_d(
      direction = -1, name = NULL,
      labels = c(`TRUE` = "Missing", `FALSE` = "Present")
    ) +
    labs(y = "Number of Species")
}

plot_miss_trait_categories_per_species_per_growth_form = function(
    species_trait_categories, combined_growth_form
) {
  species_trait_categories %>%
    mutate(across(where(is.numeric), ~ifelse(.x == 0, NA_integer_, 1L))) %>%
    janitor::clean_names(case = "title") %>%
    full_join(combined_growth_form, by = c(Species = "species")) %>%
    select(-Species) %>%
    mutate(
      growth_form = growth_form %>%
        stringr::str_to_title() %>%
        factor() %>%
        forcats::fct_infreq()
    ) %>%
    tidyr::nest(trait_cat_df = !growth_form) %>%
    arrange(growth_form) %>%
    mutate(
      trait_miss_cat_plot = purrr::map2(
        growth_form, trait_cat_df,
        ~.y %>%
          visdat::vis_miss(cluster = TRUE, sort_miss = TRUE) +
          labs(
            title = paste0(.x, "s (n=", nrow(.y), ")"),
            y = "Number of Species"
          ) +
          scale_fill_viridis_d(
            direction = -1, name = NULL,
            labels = c(`TRUE` = "Missing", `FALSE` = "Present")
          )
      )
    ) %>%
    pull(trait_miss_cat_plot) %>%
    patchwork::wrap_plots(tag_level = "new") +
    patchwork::plot_annotation(tag_levels = "A")
}

plot_miss_trait_categories_per_species_summary = function(
    species_trait_categories
) {
  species_trait_categories %>%
    mutate(across(where(is.numeric), ~ifelse(.x == 0, NA_real_, .x))) %>%
    select(-species) %>%
    janitor::clean_names("upper_camel") %>%
    ggmice::plot_pattern(rotate = TRUE) +
    theme(text = element_text(size = 10),
          axis.text.x.top = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
          axis.text.x.bottom = element_text(angle = 0, hjust = 0.5))
}

plot_number_trait_categories_per_species = function(species_trait_categories) {

  # Order species by similar trait measurements
  species_clust = species_trait_categories %>%
    as.data.frame() %>%
    tibble::column_to_rownames("species") %>%
    scale() %>%
    dist(method = "euclidean") %>%
    hclust(method = "ward.D")

  # Actual order
  species_order = species_trait_categories$species[species_clust$order]

  # Actual Figure
  species_trait_categories %>%
    tidyr::pivot_longer(
      flower:root, names_to = "trait_category", values_to = "n_traits"
    ) %>%
    mutate(
      # Order species by cluster order
      species = factor(species, levels = species_order),
      # Order trait category by number of measures of the trait
      trait_category = factor(
        trait_category,
        levels = c("life_history", "leaf", "seed", "height", "flower", "root",
                   "stem")
      )
    ) %>%
    ggplot(aes(trait_category, species, fill = n_traits)) +
    geom_raster() +
    scale_x_discrete(
      name = "Trait Category",
      labels = function(x) janitor::make_clean_names(x, case = "title")
    ) +
    scale_y_discrete(name = "Species", labels = NULL) +
    scale_fill_viridis_c(name = "Number of Traits Measured", trans = "log10") +
    theme(axis.ticks.y = element_blank(),
          legend.position = "top")
}


plot_prop_trait_per_richness = function(
    regions_trait_prop, unified_glonaf_regions
) {
  regions_trait_prop %>%
    tidyr::pivot_longer(
      !c(n_species, OBJIDsic), names_to = "prop_name", values_to = "prop_value"
    ) %>%
    # Add if region is island or not
    inner_join(
      unified_glonaf_regions %>%
        as.data.frame() %>%
        select(OBJIDsic, island),
      by = "OBJIDsic") %>%
    select(-OBJIDsic) %>%
    mutate(
      prop_name = gsub("_prop(_trait)?", "", prop_name) %>%
        factor(
          levels = c("life_history", "leaf", "seed", "height", "flower", "root",
                     "stem", "prop_with_any_trait", "has_lhs", "has_diaz",
                     "has_bergmann")
        )
    ) %>%
    ggplot(aes(n_species, prop_value, color = island == 1)) +
    geom_point(size = 1/2, alpha = 1/3) +
    facet_wrap(
      vars(prop_name),
      labeller = labeller(
        prop_name = c(
          has_bergmann     = "Root Traits\n(4 traits, Bergmann et al. 2020)",
          has_diaz         = "Aboveground traits\n(6 traits, Díaz et al. 2016)",
          has_lhs          = "Leaf-Height-Seed mass\n(3 traits, Westoby 2002)",
          prop_with_any_trait = "Any trait",
          flower              = "Flower",
          height              = "Height",
          leaf                = "Leaf",
          life_history        = "Life History",
          root                = "Root",
          seed                = "Seed",
          stem                = "Stem"
        )
      )
    ) +
    scale_x_log10("Alien Species Richness") +
    scale_y_continuous(
      "Proportion Trait(s)", labels = scales::percent_format()
    ) +
    scale_color_discrete(
      NULL,
      guide = guide_legend(override.aes = list(size = 1, alpha = 1)),
      labels = c(`TRUE` = "Island", `FALSE` = "Not Island")
    ) +
    theme_bw() +
    theme(aspect.ratio = 1, strip.background = element_blank(),
          legend.position = "top")
}

plot_proportion_known_combination_per_richness = function(
  regions_trait_prop, glonaf_species_number
) {
  regions_trait_prop %>%
    full_join(glonaf_species_number, by = "OBJIDsic") %>%
    select(starts_with("has_") | starts_with("num_")) %>%
    mutate(
      num_ratio = (num_invasive_spp + num_naturalized_spp) /
        (num_invasive_spp + num_naturalized_spp + num_native_spp)
    ) %>%
    tidyr::pivot_longer(
      has_lhs_prop:has_bergmann_prop, names_to = "comb_name",
      values_to = "comb_value"
    ) %>%
    tidyr::pivot_longer(
      starts_with("num_"), names_to = "rich_name", values_to = "rich_value"
    ) %>%
    ggplot(aes(rich_value, comb_value)) +
    geom_point(shape = ".") +
    facet_grid(
      vars(comb_name), vars(rich_name), scales = "free_x",
      labeller = labeller(
        comb_name = c(
          has_bergmann_prop = "Root Traits\n(4 traits, Bergmann et al. 2020)",
          has_diaz_prop     = "Aboveground traits\n(6 traits, Díaz et al. 2016)",
          has_lhs_prop      = "Leaf-Height-Seed mass\n(3 traits, Westoby 2002)"
        ),
        rich_name = c(
          num_invasive_spp    = "Number\nof invasive sp.",
          num_native_spp      = "Number\nof native sp.",
          num_naturalized_spp = "Number\nof naturalized sp.",
          num_ratio           = "Aliens/Natives\nrichness ratio"
        )
      )
    ) +
    ggpmisc::stat_poly_eq(formula = y ~ x, parse = TRUE, label.x = "right") +
    labs(x = "Richness value",
         caption = "1 point = 1 GloNAF region") +
    scale_y_continuous(
      "Proportion known trait combination", labels = scales::label_percent()
    ) +
    theme_bw() +
    theme(strip.background = element_blank(), aspect.ratio = 1,
          axis.text.x = element_text(angle = 25, hjust = 1))
}


# Maps -------------------------------------------------------------------------

plot_map_glonaf_regions = function(unified_glonaf_regions) {

  # Background map
  world_sf = rnaturalearth::ne_countries(returnclass = "sf") %>%
    sf::st_transform(crs = "+proj=eqearth")

  # Actual plot
  unified_glonaf_regions %>%
    ggplot() +
    geom_sf(data = world_sf, fill = "gray85", color = "gray55", size = 1/2) +
    geom_sf(fill = "darkblue", alpha = 1/3, size = 1/5) +
    labs(title = "Map of GloNAF regions of the world",
         subtitle = "Darker regions means overlapping selected regions") +
    theme_void() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
}

plot_map_proportion_trait_by_region = function(
    regions_trait_prop, glonaf_small_islands, glonaf_mainland_large_islands
) {
  # Background map
  world_sf = rnaturalearth::ne_countries(returnclass = "sf") %>%
    sf::st_transform(crs = "+proj=eqearth")

  # Pivot trait data to be usable across facets
  pivoted_data = regions_trait_prop %>%
    select(OBJIDsic, prop_with_any_trait:has_bergmann_prop) %>%
    tidyr::pivot_longer(
      !OBJIDsic, names_to = "prop_name", values_to = "prop_value"
    ) %>%
    mutate(
      prop_name = factor(
        prop_name,
        levels = c("prop_with_any_trait", "has_lhs_prop", "has_diaz_prop",
                   "has_bergmann_prop")
      )
    )

  # Actual plot
  glonaf_mainland_large_islands %>%
    inner_join(pivoted_data, by = "OBJIDsic") %>%
    ggplot(aes(fill = prop_value)) +
    geom_sf(data = world_sf, fill = "gray85", color = "gray65", size = 1/100) +
    # Non-small islands and mainlands
    geom_sf(color = "gray65", size = 1/100) +
    # Small islands
    geom_sf(
      aes(color = prop_value),
      fill = NA,
      data = glonaf_small_islands %>%
        inner_join(pivoted_data, by = "OBJIDsic"),
      size = 2.5, shape = 21, stroke = 1.5
    ) +
    facet_wrap(
      vars(prop_name),
      labeller = labeller(
        prop_name = c(
          has_bergmann_prop = "Root Traits\n(4 traits, Bergmann et al. 2020)",
          has_diaz_prop     = "Aboveground traits\n(6 traits, Díaz et al. 2016)",
          has_lhs_prop      = "Leaf-Height-Seed mass\n(3 traits, Westoby 2002)",
          prop_with_any_trait = "Any trait"
        )
      )
    ) +
    scale_fill_viridis_c(
      name = "Prop. of aliens species\nwith trait combination",
      labels = scales::percent_format()
    ) +
    scale_color_viridis_c(
      name = "Prop. of aliens species\nwith trait combination",
      labels = scales::percent_format()
    ) +
    ylim(-5747986, NA) +  # Remove whatever is below 60°S
    theme_void() +
    theme(legend.position = "top", strip.background = element_blank())
}


plot_map_proportion_trait_by_organ_by_region = function(
    regions_trait_prop, glonaf_small_islands, glonaf_mainland_large_islands
) {
  # Background map
  world_sf = rnaturalearth::ne_countries(returnclass = "sf") %>%
    sf::st_transform(crs = "+proj=eqearth")

  # Pivot trait data to be usable across facets
  pivoted_data = regions_trait_prop %>%
    select(OBJIDsic, leaf_prop_trait:root_prop_trait) %>%
    tidyr::pivot_longer(
      !OBJIDsic, names_to = "prop_name", values_to = "prop_value"
    ) %>%
    mutate(
      prop_name = gsub("_prop_trait", "", prop_name, fixed = TRUE)
    )

  # Actual plot
  glonaf_mainland_large_islands %>%
    inner_join(pivoted_data, by = "OBJIDsic") %>%
    ggplot(aes(fill = prop_value)) +
    geom_sf(data = world_sf, fill = "gray85", color = "gray65", size = 1/100) +
    # Non-small islands and mainlands
    geom_sf(color = "gray65", size = 1/100) +
    # Small islands
    geom_sf(
      aes(color = prop_value),
      fill = NA,
      data = glonaf_small_islands %>%
        inner_join(pivoted_data, by = "OBJIDsic"),
      size = 2.5, shape = 21, stroke = 1.5
    ) +
    facet_wrap(
      vars(prop_name),
      labeller = labeller(
        prop_name = snakecase::to_title_case
      )
    ) +
    scale_fill_viridis_c(
      name = "Prop. of aliens species\nwith at least one trait per organ",
      labels = scales::percent_format()
    ) +
    scale_color_viridis_c(
      name = "Prop. of aliens species\nwith at least one trait per organ",
      labels = scales::percent_format()
    ) +
    ylim(-5747986, NA) +  # Remove whatever is below 60°S
    theme_void() +
    theme(legend.position = "top", strip.background = element_blank())
}

#' Plot a map of alien species richness per region
#'
#' @noRd
plot_map_alien_richness_region = function(
    regions_trait_prop, glonaf_small_islands,
    glonaf_mainland_large_islands
) {
  # Background map
  world_sf = rnaturalearth::ne_countries(returnclass = "sf") %>%
    sf::st_transform(crs = "+proj=eqearth")

  region_richness = regions_trait_prop %>%
    select(OBJIDsic, n_species)

  # Actual plot
  glonaf_mainland_large_islands %>%
    inner_join(region_richness, by = "OBJIDsic") %>%
    ggplot(aes(fill = n_species)) +
    geom_sf(data = world_sf, fill = "gray85", size = 1/100) +
    # Non-small islands and mainlands
    geom_sf() +
    # Small islands
    geom_sf(
      aes(color = n_species),
      fill = NA,
      data = glonaf_small_islands %>%
        inner_join(region_richness, by = "OBJIDsic"),
      size = 2.5, shape = 21, stroke = 1.5
    ) +
    scale_fill_viridis_b(
      name = "Alien Species Richness", trans = "log10", n.breaks = 5,
      show.limits = TRUE,
      # Force limit to merge axes
      limits = range(region_richness[["n_species"]])
    ) +
    scale_color_viridis_b(
      name = "Alien Species Richness", trans = "log10", n.breaks = 5,
      show.limits = TRUE,
      # Force limit to merge axes
      limits = range(region_richness[["n_species"]])
    ) +
    ylim(-5747986, NA) +  # Remove whatever is below 60°S
    theme_void() +
    theme(legend.position = "top",
          legend.key.width = unit(2, "lines"))
}


plot_map_europe_proportion_trait = function(
    regions_trait_prop, glonaf_small_islands, glonaf_mainland_large_islands
) {
  # Background map
  europe_sf = rnaturalearth::ne_countries(returnclass = "sf") %>%
    sf::st_transform(crs = "EPSG:4258") %>%
    filter(continent == "Europe")


  # Pivot trait proportions data
  pivot_trait = regions_trait_prop %>%
    select(-n_species) %>%
    tidyr::pivot_longer(
      !OBJIDsic, names_to = "prop_name", values_to = "prop_value"
    ) %>%
    mutate(
      prop_name = factor(
        prop_name,
        levels = c(
          "prop_with_any_trait", "has_lhs_prop", "has_diaz_prop",
          "has_bergmann_prop", "flower_prop_trait", "height_prop_trait",
          "leaf_prop_trait", "life_history_prop_trait", "root_prop_trait",
          "seed_prop_trait", "stem_prop_trait")
        )
    )

  glonaf_map_mainland = glonaf_mainland_large_islands %>%
    sf::st_transform(sf::st_crs("EPSG:4258")) %>%
    inner_join(pivot_trait, by = "OBJIDsic")

  glonaf_map_islands = glonaf_small_islands %>%
    sf::st_transform(sf::st_crs("EPSG:4258")) %>%
    inner_join(pivot_trait, by = "OBJIDsic")

  ggplot() +
    # Background map
    geom_sf(data = europe_sf, fill = "gray45", alpha = 1/3) +
    # Mainland
    geom_sf(data = glonaf_map_mainland, aes(fill = prop_value)) +
    # Islands
    geom_sf(data = glonaf_map_islands, fill = NA, size = 2.5, shape = 21,
            stroke = 1.5, aes(color = prop_value)) +
    # Rest
    facet_wrap(
      vars(prop_name),
      labeller = labeller(
        prop_name = c(
          has_bergmann_prop = "Root Traits\n(4 traits, Bergmann et al. 2020)",
          has_diaz_prop     = "Aboveground traits\n(6 traits, Díaz et al. 2016)",
          has_lhs_prop      = "Leaf-Height-Seed mass\n(3 traits, Westoby 2002)",
          prop_with_any_trait = "Any trait",
          flower_prop_trait = "Prop. Aliens\nFlower",
          height_prop_trait = "Prop. Aliens\nHeight",
          leaf_prop_trait = "Prop. Aliens\nLeaf",
          life_history_prop_trait = "Prop. Aliens\nLife History",
          root_prop_trait = "Prop. Aliens\nRoot",
          seed_prop_trait = "Prop. Aliens\nSeed",
          stem_prop_trait = "Prop. Aliens\nStem"
        )
      )
    ) +
    coord_sf(
      xlim = c(-22, 40),
      ylim = c(36, 70)
    ) +
    scale_fill_viridis_c(
      "Prop. Alien Species with trait", labels = scales::percent_format(),
      limits = c(0, 1)
    ) +
    scale_color_viridis_c(
      "Prop. Alien Species with trait", labels = scales::percent_format(),
      limits = c(0, 1)
    ) +
    ggthemes::theme_map() +
    theme(
      legend.position = "top",
      legend.direction = "horizontal",
      strip.background = element_blank()
    )

}
