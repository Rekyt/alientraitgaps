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
    theme(legend.position = "top")
}
