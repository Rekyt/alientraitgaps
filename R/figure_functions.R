# Functions to plot figures
# Trait Counts -----------------------------------------------------------------

plot_number_species_per_trait_combined = function(
    combined_traits, glonaf_tnrs
) {

  max_25_traits = combined_traits %>%
    count(consolidated_name, sort = TRUE, name = "n_species") %>%
    filter(
      !grepl("Elevational", consolidated_name, fixed = TRUE),
      !grepl("Habitat", consolidated_name, fixed = TRUE),
      !grepl(
        "Species occurrence range: native vs invasive", consolidated_name,
        fixed = TRUE
      )
    ) %>%
    slice_head(n = 25)

  total_sp = glonaf_tnrs %>%
    filter(Accepted_species != "") |>
    distinct(species = Accepted_species) |>
    pull(species) %>%
    unique() %>%
    length()

  # Clean environment
  rm(combined_traits)

  # Actual plot
  max_25_traits %>%
    ggplot(aes(n_species, forcats::fct_reorder(consolidated_name, n_species))) +
    # 50% vertical line
    geom_text(
      label = "50%", color = "darkblue", x = total_sp/2, y = 25, hjust = 0.5,
      vjust = -2.2, size = rel(4.2)
    ) +
    geom_vline(
      xintercept = total_sp/2, linetype = 2, color = "darkblue", linewidth = 1
    ) +
    # 100% vertical line
    geom_vline(
      xintercept = total_sp, linetype = 2, color = "firebrick", linewidth = 1
    ) +
    geom_text(
      label = "100%", color = "firebrick", x = total_sp, y = 25, hjust = 0.5,
      vjust = -2.2, size = rel(4.2)
    ) +
    # Actual geoms
    geom_text(
      aes(label = paste0(round((n_species/total_sp) * 100, 0), "%")),
      hjust = -0.1, size = 4.2, vjust = 0
    ) +
    geom_point(size = 1.2) +
    # Scales and Themes
    scale_x_continuous(
      name = "Number of species with traits",
      sec.axis = sec_axis(
        trans = ~.x/total_sp, labels = scales::percent_format(),
        breaks = c(0.4, 0.6, 0.8)
      )
    ) +
    scale_y_discrete(
      name = "Trait name",
      labels = c(
        `Plant growth form`                = "Growth form (cat.)",
         Woodiness                         = "Woodiness (cat.)",
        `Life history`                     = "Life history (cat.)",
        `Plant vegetative height`          = "Plant height (cont.)",
        `Leaf type`                        = "Leaf type (cat.)",
        `Leaf compoundness`                = "Leaf compoundness (cat.)",
        `Diaspore dispersal syndrome`      = "Diaspore dispersal syndrome (cat.)",
        `Seed dry mass`                    = "Seed mass (cont.)",
        `Plant photosynthetic pathway`     = "Photosynthetic pathway (cat.)",
        `Flowering time, by month`         = "Flowering phenology (cat.)",
        `Plant nitrogen fixation capacity` = "Nitrogen fixer (bin.)",
        `Fruit type`                       = "Fruit type (cat.)",
        `Leaflet number per leaf`          = "Leaflet number per leaf (cont.)",
        `Leaf length`                      = "Leaf length (cont.)",
        `Leaf phenology`                   = "Leaf phenology (cat.)",
        `Species tolerance to frost`       = "Frost tolerance (cat.)",
        `Leaf phyllotaxis`                 = "Leaf phyllotaxis (cat.)",
        `Leaf width`                       = "Leaf width (cont.)",
        `Pollination syndrome`             = "Pollination syndrome (cat.)",
        `Fruit length`                     = "Fruit length (cont.)",
        `Plant sex type`                   = "Plant sex type (cat.)",
        `Flower colour`                    = "Flower colour (cat.)",
        `Leaf mass per area`               = "Leaf mass per area (cont.)",
        `Leaf area`                        = "Leaf area (cont.)",
        `Seed germination proportion`      = "Seed Germination Rate (cont.)"
      )
    ) +
    theme_bw(16) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    coord_cartesian(clip = "off")

}



plot_number_specific_trait_combination = function(contain_trait_combination) {

  # Pre-process dataset
  prop_comb_sp = contain_trait_combination %>%
    select(-traits) %>%
    tidyr::pivot_longer(!species, names_to = "comb_name",
                        values_to = "comb_value") %>%
    filter(comb_value) %>%
    count(comb_name, sort = TRUE, name = "n_species") %>%
    filter(comb_name != "in_glonaf")

  n_sp = nrow(contain_trait_combination)

  # Clean environment
  rm(contain_trait_combination)

  # Actual Plot
  prop_comb_sp %>%
    ggplot(
      aes(
        n_species,
        forcats::fct_relevel(comb_name, "has_bergmann", "has_diaz", "has_lhs",
                             "has_at_least_one_trait")
      )
    ) +
    geom_vline(
      xintercept = n_sp, linetype = 2, size = 1,
      color = "darkred"
    ) +
    geom_point(size = 2, color = "darkblue") +
    geom_text(
      aes(
        label = paste0(
          round(n_species/n_sp * 100, 1), "%"
        )
      ), hjust = -0.2, vjust = 0.5
    ) +
    scale_x_continuous(
      sec.axis = sec_axis(
        ~./n_sp, labels = scales::percent_format()
      )
    ) +
    scale_y_discrete(
      labels = c(
        in_glonaf = "In GloNAF",
        has_at_least_one_trait = "At least one trait",
        has_lhs   = "LHS\n(3 traits, Westoby 1998)",
        has_diaz  = "Aboveground spectrum\n(6 traits, Díaz et al., 2016)",
        has_bergmann = "Root traits\n(4 traits, Bergmann et al., 2020)"
      )
    ) +
    labs(x = "Number of species",
         y = "Trait Combination") +
    theme_bw()
}


plot_trait_comb_proportion_per_invasion_status = function(
    glonaf_status_trait_cat
) {

  # Pre-process data
  trait_comb_prop_status = glonaf_status_trait_cat %>%
    mutate(
      is_invasive_or_never = case_when(
        invasive == 0 & naturalized == 0 ~ "native",
        invasive > 0                     ~ "invasive",
        naturalized  > 0                 ~ "naturalized"
      )
    ) %>%
    group_by(is_invasive_or_never) %>%
    summarise(
      across(
        has_at_least_one_trait:has_bergmann,
        .fns = list(
          prop   = ~sum(.x, na.rm = TRUE)/n(),
          n_true = ~sum(.x, na.rm = TRUE)
        )
      ),
      n = n(),
    )

  status_labels = trait_comb_prop_status %>%
    select(is_invasive_or_never, n) %>%
    mutate(
      better_status = case_when(
        is_invasive_or_never == "native"      ~ "Native",
        is_invasive_or_never == "invasive"    ~ "Ref. invasive ≥ 1",
        is_invasive_or_never == "naturalized" ~ "Naturalized never invasive"
      ),
      labels = paste0(better_status, "\n(n = ", format(n, big.mark = ","), ")")
    ) %>%
    select(-n, -better_status) %>%
    tibble::deframe()

  trait_comb_prop_status = trait_comb_prop_status %>%
    select(is_invasive_or_never, ends_with("prop")) %>%
    tidyr::pivot_longer(
      ends_with("prop"), names_to = "comb_name", values_to = "comb_value"
    ) %>%
    mutate(
      is_invasive_or_never = factor(
        is_invasive_or_never, levels = c("native", "naturalized", "invasive")
      ),
      comb_name = factor(
        comb_name,
        levels = c("has_bergmann_prop", "has_diaz_prop", "has_lhs_prop",
                   "has_at_least_one_trait_prop")
      )
    )

  # Clean environment
  rm(glonaf_status_trait_cat)


  # Actual Plot
  trait_comb_prop_status %>%
    ggplot(
      aes(comb_name, comb_value, shape = is_invasive_or_never,
          color = is_invasive_or_never)
    ) +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    geom_linerange(
      aes(x = comb_name, ymin = 0, ymax = comb_value,
          group = interaction(is_invasive_or_never, comb_name)),
      position = position_dodge(width = 0.5)
    ) +
    scale_x_discrete(
      "Trait Combination",
      labels = c(
        has_bergmann_prop           = "Root traits",
        has_diaz_prop               = "Aboveground\nspectrum",
        has_lhs_prop                = "LHS traits",
        has_at_least_one_trait_prop = "At least\none trait"
      )
    ) +
    scale_y_continuous(
      "Proportion of Species", labels = scales::label_percent(),
      limits = c(0, 1)
    ) +
    scale_shape_discrete(
      "Species Status", guide = guide_legend(reverse = TRUE),
      labels = status_labels
    ) +
    scale_color_manual("Species Status", values = c(
      native = "#009E73", invasive = "#E69F00", naturalized = "#56B4E9"
    ), guide = guide_legend(reverse = TRUE), labels = status_labels
    ) +
    coord_flip() +
    theme_bw() +
    theme(
      legend.position = "top",
      legend.title.position = "top",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
    )
}


plot_trait_combination_per_range_size = function(
    glonaf_species_area, contain_trait_combination
) {

  # Pre-process data
  trait_comb_prop_widespread = glonaf_species_area %>%
    select(-total_area) %>%
    arrange(desc(n_regions)) %>%
    mutate(
      n_regions_rank = row_number(),
      n_regions_top  = n_regions_rank <= 100
    ) %>%
    left_join(
      contain_trait_combination %>%
        select(-traits, -in_glonaf)
    ) %>%
    group_by(n_regions_top) %>%
    summarise(
      across(
        has_at_least_one_trait:has_bergmann,
        .fns = list(
          prop   = ~sum(.x, na.rm = TRUE)/n(),
          n_true = ~sum(.x, na.rm = TRUE)
        )
      ),
      n = n(),
    ) %>%
    # Reformat proportion in a tidy format
    select(n_regions_top, ends_with("prop")) %>%
    tidyr::pivot_longer(
      ends_with("prop"), names_to = "comb_name", values_to = "comb_value"
    ) %>%
    mutate(
      comb_name = factor(
        comb_name,
        levels = c("has_bergmann_prop", "has_diaz_prop", "has_lhs_prop",
                   "has_at_least_one_trait_prop")
      )
    )

  # Clean environment
  rm(glonaf_species_area, contain_trait_combination)

  # Actual Plot
  trait_comb_prop_widespread %>%
    ggplot(
      aes(comb_name, comb_value, shape = n_regions_top, color = n_regions_top)
    ) +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    geom_linerange(
      aes(x = comb_name, ymin = 0, ymax = comb_value,
          group = interaction(n_regions_top, comb_name)),
      position = position_dodge(width = 0.5)
    ) +
    scale_x_discrete(
      "Trait Combination",
      labels = c(
        has_bergmann_prop           = "Root traits",
        has_diaz_prop               = "Aboveground\nspectrum",
        has_lhs_prop                = "LHS traits",
        has_at_least_one_trait_prop = "At least\none trait"
      )
    ) +
    scale_y_continuous(
      "Proportion of Species", labels = scales::label_percent(),
      limits = c(0, 1)
    ) +
    scale_shape_discrete(
      "Widespread species?",
      guide = guide_legend(reverse = TRUE),
      labels = c(`TRUE` = "Yes", `FALSE` = "No")
    ) +
    scale_color_manual(
      "Widespread species?",
      guide = guide_legend(reverse = TRUE),
      labels = c(`TRUE` = "Yes", `FALSE` = "No"),
      values = c(`TRUE` = "#018571", `FALSE` = "#a6611a")
    ) +
    coord_flip() +
    theme_bw() +
    theme(
      legend.position = "top",
      legend.title.position = "top",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
    )
}

plot_number_of_traits_per_number_of_regions = function(
  contain_trait_combination, glonaf_species_area
) {
  # Preprocess data
  n_traits_n_regions = contain_trait_combination %>%
    mutate(n_traits = purrr::map_int(traits, length)) %>%
    select(species, n_traits) %>%
    ungroup() %>%
    inner_join(glonaf_species_area, by = "species")

  # Clean plotting environment
  rm(contain_trait_combination, glonaf_species_area)

  # Actual plot
  n_traits_n_regions %>%
    ggplot(aes(n_regions, n_traits)) +
    geom_point(shape = ".") +
    stat_smooth(method = "lm", se = FALSE) +
    scale_x_log10("# of Occupied Regions") +
    scale_y_log10("# of Traits") +
    ggpmisc::stat_poly_eq(formula = y ~ x, label.x = 0.9) +
    theme_bw()
}

# Taxonomic Treemaps -----------------------------------------------------------

plot_taxonomy_treemap_trait_combination = function(
    trait_combinations_full, glonaf_tnrs
) {

  # Pre-process data
  tax_comb = glonaf_tnrs |>
    filter(Accepted_species != "") |>
    distinct(species = Accepted_species, family = Accepted_family) |>
    mutate(genus = stringr::str_extract(species, "^\\w+")) |>
    distinct(species, genus, family) %>%
    inner_join(
      trait_combinations_full %>%
        select(-traits),
      by = "species") |>
    mutate(
      across(where(is.character), ~iconv(.x, "latin1", to = "UTF-8")),
      trait_category = interaction(
        has_at_least_one_trait, has_lhs, has_diaz, has_bergmann
      )) %>%
    count(family, trait_category, name = "n_species") %>%
    group_by(family) %>%
    mutate(prop = 100 * n_species/sum(n_species)) %>%
    ungroup() %>%
    mutate(
      plot_label = format(n_species, big.mark = ",", trim = TRUE)
    )

  # Clean environment
  rm(trait_combinations_full, glonaf_tnrs)

  # Actual Plot
  tax_comb %>%
    # Account for species with no family to avoid error in 'ggfittext'
    mutate(family = ifelse(family == "", "Other", family)) %>%
    # Rest of the plot
    ggplot(
      aes(
        area  = n_species, fill = trait_category, label = plot_label,
        subgroup = family, subgroup2 = trait_category
      )
    ) +
    # Fill the trait combination squares
    treemapify::geom_treemap(color = NA) +
    # Organize by family
    treemapify::geom_treemap_subgroup_border(
      show.legend = FALSE, size = 2, color = "white"
    ) +
    # Organize by trait combination
    treemapify::geom_treemap_subgroup2_border(
      show.legend = FALSE, size = 1/3, color = "white"
    ) +
    # Add numbers in tiles
  treemapify::geom_treemap_text(
    aes(
      color = trait_category %in% c(
        "TRUE.TRUE.FALSE.TRUE", "TRUE.TRUE.TRUE.FALSE", "TRUE.TRUE.TRUE.TRUE"
      )
    ),
    show.legend = FALSE, size = 7.5
  ) +
    # Add family labels
    treemapify::geom_treemap_subgroup_text(
      place = "centre", grow = TRUE, alpha = 4/5, colour = "black",
      fontface = "bold.italic", min.size = 1, show.legend = FALSE
    ) +
    # Scales & theme
    scale_fill_manual(
      name = "Trait combination",
      labels = c(
        "FALSE.FALSE.FALSE.FALSE" = "No trait",
        "TRUE.FALSE.FALSE.FALSE"  = "At least\none trait",
        "TRUE.FALSE.FALSE.TRUE"   = "Root traits",
        "TRUE.TRUE.FALSE.FALSE"   = "LHS",
        "TRUE.TRUE.FALSE.TRUE"    = "LHS\n& Root traits",
        "TRUE.TRUE.TRUE.FALSE"    = "Aboveground\nspectrum",
        "TRUE.TRUE.TRUE.TRUE"     = "Aboveground\nspectrum\n& Root traits"
      ),
      values = c(
        "FALSE.FALSE.FALSE.FALSE" = "#f0f0f0",  # No trait
        "TRUE.FALSE.FALSE.FALSE"  = "#d3d3d3",  # >=1 trait
        "TRUE.FALSE.FALSE.TRUE"   = "#d25601",  # Root traits
        "TRUE.TRUE.FALSE.FALSE"   = "#9283ac",  # LHS
        "TRUE.TRUE.FALSE.TRUE"    = "#923601",  # LHS + Root
        "TRUE.TRUE.TRUE.FALSE"    = "#563787",  # Aboveground
        "TRUE.TRUE.TRUE.TRUE"     = "#551601"   # Aboveground + Root
      )
    ) +
    scale_color_manual(
      values = c(`TRUE` = "white", `FALSE` = "black"), guide = NULL
    ) +
    # Make the plot squared
    theme(legend.position = "top", aspect.ratio = 1)
}

plot_taxonomy_treemap_number_traits = function(
    trait_combinations_full, glonaf_tnrs, logged = TRUE
) {

  # Pre-process data
  tax_comb = glonaf_tnrs |>
    filter(Accepted_species != "") |>
    distinct(species = Accepted_species, family = Accepted_family) |>
    inner_join(trait_combinations_full, by = "species") %>%
    mutate(
      n_traits = purrr::map_int(
        traits, \(x) ifelse(all(is.na(x)), 0, length(x))
      )
    ) %>%
    mutate(across(where(is.character), ~iconv(.x, "latin1", to = "UTF-8")))

  if (logged) {
    color_scale = scale_fill_viridis_b(
      name = "Number of traits",
      trans = "log10",
      show.limits = TRUE,
      limits = range(tax_comb$n_traits) + 1
    )
  } else {
    color_scale = scale_fill_viridis_b(
      name = "Number of traits",
      show.limits = TRUE,
      limits = range(tax_comb$n_traits) + 1
    )
  }

  # Clean environment
  rm(trait_combinations_full, glonaf_tnrs)

  # Actual plot
  tax_comb %>%
    ggplot(
      aes(area = 1, fill = n_traits + 1, subgroup = family, subgroup2 = genus)
    ) +
    treemapify::geom_treemap(color = NA) +
    treemapify::geom_treemap_subgroup_border(size = 0.5, color = "white") +
    treemapify::geom_treemap_subgroup_text(
      place = "centre", grow = TRUE, alpha = 0.8, colour = "white",
      fontface = "italic", min.size = 0
    ) +
    color_scale +
    theme(
      aspect.ratio = 1,
      legend.position = "top"
    )

}

plot_general_treemap_trait_combination = function(
    trait_combinations_full, glonaf_tnrs
) {

  # Preprocess data
  tax_comb = glonaf_tnrs |>
    filter(Accepted_species != "") |>
    distinct(species = Accepted_species, family = Accepted_family) %>%
    inner_join(
      trait_combinations_full %>%
        select(-traits),
      by = "species") %>%
    mutate(across(where(is.character), ~iconv(.x, "latin1", to = "UTF-8"))) %>%
    mutate(
      trait_category = interaction(
        has_at_least_one_trait, has_lhs, has_diaz, has_bergmann
      )
    ) %>%
    count(trait_category, name = "n_species")

  total = sum(tax_comb[["n_species"]])

  tax_comb = tax_comb %>%
    mutate(
      prop       = 100 * (n_species/total),
      plot_label = ifelse(
        round(prop, 0) > 0,
        paste0(
          "n = ", format(n_species, big.mark = ",", trim = TRUE),
          "\n(", round(prop, 0), "%)"
        ),
        paste0(
          "n = ", format(n_species, big.mark = ",", trim = TRUE),
          "\n(", round(prop, 1), "%)"
        )
      )
    )

  # Clean environment
  rm(trait_combinations_full, glonaf_tnrs)

  # Actual plot
  tax_comb %>%
    ggplot(
      aes(
        area  = n_species, fill = trait_category, label = plot_label,
        subgroup = trait_category
      )
    ) +
    treemapify::geom_treemap(color = NA) +
    treemapify::geom_treemap_text(
      aes(
        color = trait_category %in% c(
          "TRUE.TRUE.FALSE.TRUE", "TRUE.TRUE.TRUE.FALSE", "TRUE.TRUE.TRUE.TRUE"
        )
      ),
      place = "center", show.legend = FALSE,
      padding.x = grid::unit(1/2, "mm"), padding.y = grid::unit(1/2, "mm")
    ) +
    # Theme & Scales
    scale_fill_manual(
      name = "Trait combination",
      labels = c(
        "FALSE.FALSE.FALSE.FALSE" = "No trait",
        "TRUE.FALSE.FALSE.FALSE"  = "At least\none trait",
        "TRUE.FALSE.FALSE.TRUE"   = "Root traits",
        "TRUE.TRUE.FALSE.FALSE"   = "LHS",
        "TRUE.TRUE.FALSE.TRUE"    = "LHS\n& Root traits",
        "TRUE.TRUE.TRUE.FALSE"    = "Aboveground\nspectrum",
        "TRUE.TRUE.TRUE.TRUE"     = "Aboveground\nspectrum\n& Root traits"
      ),
      values = c(
        "FALSE.FALSE.FALSE.FALSE" = "#f0f0f0",  # No trait
        "TRUE.FALSE.FALSE.FALSE"  = "#d3d3d3",  # >=1 trait
        "TRUE.FALSE.FALSE.TRUE"   = "#d25601",  # Root traits
        "TRUE.TRUE.FALSE.FALSE"   = "#9283ac",  # LHS
        "TRUE.TRUE.FALSE.TRUE"    = "#923601",  # LHS + Root
        "TRUE.TRUE.TRUE.FALSE"    = "#563787",  # Aboveground
        "TRUE.TRUE.TRUE.TRUE"     = "#551601"   # Aboveground + Root
      )
    ) +
    scale_color_manual(
      values = c(`TRUE` = "white", `FALSE` = "black"), guide = NULL
    ) +
    theme(legend.position = "top", aspect.ratio = 1)
}

# Missing Traits ---------------------------------------------------------------

plot_combined_traits_heatmap = function(
    simplified_traits_full, glonaf_tnrs, first_n_traits = Inf
) {

  # Get all combinations of trait species which shows which has values
  comb_sp = simplified_traits_full %>%
    mutate(value = TRUE) %>%
    full_join(
      glonaf_tnrs %>%
        filter(Accepted_species != "") |>
        distinct(species = Accepted_species) %>%
        tidyr::crossing(
          consolidated_name = unique(simplified_traits_full[["consolidated_name"]])
        ),
      by = c("species", "consolidated_name")
    ) %>%
    mutate(value = ifelse(is.na(value), FALSE, value))

  # Species rank
  sp_rank = comb_sp %>%
    group_by(species) %>%
    summarise(n_values = sum(value)) %>%
    arrange(desc(n_values)) %>%
    mutate(
      species_fact = forcats::fct_reorder(
        species, n_values, max, .desc = TRUE
      ),
      species_rank = as.numeric(species_fact)
    )

  # Trait ranks
  trait_rank = comb_sp %>%
    group_by(consolidated_name) %>%
    summarise(n_values = sum(value)) %>%
    arrange(desc(n_values)) %>%
    mutate(
      trait_fact = forcats::fct_reorder(
        consolidated_name, n_values, max, .desc = TRUE
      ),
      trait_rank = as.numeric(trait_fact)
    )

  # Get combinations of trait measured by species ordered by frequency
  comb_sp_freq = comb_sp %>%
    inner_join(sp_rank, by = "species") %>%
    inner_join(trait_rank, by = "consolidated_name") %>%
    distinct(species_rank, trait_rank, value)

  # Clean environment
  rm(simplified_traits_full, sp_rank, trait_rank, glonaf_tnrs)

  # Plot as a heatmap
  comb_sp_freq %>%
    filter(trait_rank <= first_n_traits) |>
    ggplot(aes(trait_rank, species_rank, fill = value)) +
    geom_raster() +
    labs(
      x    = "Trait Rank",
      y    = "Species Rank",
      fill = "Was trait measured?"
    ) +
    scale_fill_manual(labels = c(`TRUE` = "Yes", `FALSE` = "No"),
                      values = c(`TRUE` = "#222222", `FALSE` = "white")) +
    coord_cartesian(expand = FALSE) +
    theme_bw(16) +
    theme(
      legend.position = "top",
      legend.key = element_rect(colour = "#222222")
    )
}


plot_inset_trait_heatmap = function(
    main_heatmap, inset_heatmap
) {

  inset_heatmap = inset_heatmap +
    # Remove junk to make simpler inset
    theme(
      axis.text.y     = element_blank(),
      axis.ticks.y    = element_blank(),
      axis.title      = element_blank(),
      plot.background = element_blank(),
      legend.position = "none"
    )

  main_heatmap +
    patchwork::inset_element(inset_heatmap, 0.6, 0.6, 1, 1)

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

  # Mainland regions
  mainland_pivot = glonaf_mainland_large_islands %>%
    inner_join(pivoted_data, by = "OBJIDsic")

  # Island regions
  island_pivot = glonaf_small_islands %>%
    inner_join(pivoted_data, by = "OBJIDsic")

  # Clean environment
  rm(pivoted_data, glonaf_mainland_large_islands, glonaf_small_islands,
     regions_trait_prop)

  # Actual plot
  mainland_pivot %>%
    ggplot(aes(fill = prop_value)) +
    geom_sf(
      data = world_sf |>
              filter(continent != "Antarctica"),
      fill = "gray85", color = "gray65", size = 1/100
    ) +
    # Non-small islands and mainlands
    geom_sf(color = NA, size = 1/100) +
    # Small islands
    geom_sf(
      aes(color = prop_value),
      fill = NA,
      data = island_pivot,
      size = 1.2, shape = 21, stroke = 0.4
    ) +
    facet_wrap(
      vars(prop_name),
      labeller = labeller(
        prop_name = c(
          has_bergmann_prop   = "Root Traits",
          has_diaz_prop       = "Aboveground spectrum",
          has_lhs_prop        = "LHS",
          prop_with_any_trait = "Any trait"
        )
      )
    ) +
    scale_fill_viridis_b(
      name = "Proportion of measured alien plants",
      labels = scales::percent_format(), n.breaks = 6, show.limits = TRUE
    ) +
    scale_color_viridis_b(
      name = "Proportion of measured alien plants",
      labels = scales::percent_format(), n.breaks = 6, show.limits = TRUE
    ) +
    guides(fill = guide_colorsteps(title.vjust = 0.8),
           color = guide_colorsteps(title.vjust = 0.8)) +
    theme_void() +
    theme(
      legend.position = "bottom",
      legend.key.width = unit(1.5, "lines"),
      legend.key.height = unit(0.5, "lines"),
      panel.grid.major = element_line(color = "lightgrey", linetype = 3),
      strip.background = element_blank(),
      strip.clip = "off"
    )

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

  # Alien Species richness per GloNAF region
  region_richness = regions_trait_prop %>%
    select(OBJIDsic, n_species)

  richness_range = range(region_richness[["n_species"]])

  # Mainland richness
  mainland_richness = glonaf_mainland_large_islands %>%
    inner_join(region_richness, by = "OBJIDsic")

  # Island richness
  island_richness = glonaf_small_islands %>%
    inner_join(region_richness, by = "OBJIDsic")

  # Clean environment
  rm(regions_trait_prop, glonaf_small_islands, glonaf_mainland_large_islands,
     region_richness)

  # Actual plot
  mainland_richness %>%
    ggplot(aes(fill = n_species)) +
    geom_sf(
      data = world_sf |>
        filter(continent != "Antarctica"),
      fill = "gray85", color = "gray65", size = 1/100
    ) +
    # Non-small islands and mainlands
    geom_sf() +
    # Small islands
    geom_sf(
      aes(color = n_species),
      fill = NA,
      data = island_richness,
      size = 1.2, shape = 21, stroke = 0.5
    ) +
    scale_fill_fermenter(
      name = "Alien Plant Richness", trans = "log10", n.breaks = 5,
      palette = "YlOrRd", direction = 1,
      show.limits = TRUE,
      # Force limit to merge axes
      limits = richness_range
    ) +
    scale_color_fermenter(
      name = "Alien Plant Richness", trans = "log10", n.breaks = 5,
      palette = "YlOrRd", direction = 1,
      show.limits = TRUE,
      # Force limit to merge axes
      limits = richness_range
    ) +
    theme_void() +
    guides(fill = guide_colorsteps(title.vjust = 0.8),
           color = guide_colorsteps(title.vjust = 0.8)) +
    theme(
      legend.position  = "bottom",
      legend.key.width = unit(1.5, "lines"),
      legend.key.height = unit(0.5, "lines"),
      panel.grid.major = element_line(color = "lightgrey", linetype = 3)
    )
}


plot_map_median_n_traits_region = function(
  trait_n_regions, glonaf_small_islands,
  glonaf_mainland_large_islands_simplified
) {

  # Background Map
  world_sf = rnaturalearth::ne_countries(returnclass = "sf") %>%
    sf::st_transform(crs = "+proj=eqearth")

  # Mainland median traits
  mainland_n_traits = glonaf_mainland_large_islands_simplified %>%
    inner_join(trait_n_regions, by = "OBJIDsic")

  # Island median traits
  island_n_traits = glonaf_small_islands %>%
    inner_join(trait_n_regions, by = "OBJIDsic")

  # Clean environment
  rm(trait_n_regions, glonaf_small_islands,
     glonaf_mainland_large_islands_simplified)

  # Actual Plot
  mainland_n_traits %>%
    ggplot(aes(fill = n_traits_median)) +
    geom_sf(
      data = world_sf |>
        filter(continent != "Antarctica"),
      fill = "gray85", color = "gray65", size = 1/100
    ) +
    # Non-small islands and mainlands
    geom_sf(color = NA, size = 1/100) +
    # Small islands
    geom_sf(
      aes(color = n_traits_median),
      fill = NA,
      data = island_n_traits,
      size = 3, shape = 21, stroke = 2
    ) +
    # Fixed manual breaks to sync color & fill scales
    scale_fill_viridis_b(
      name = "Median Number of Traits\nacross Species per Region",
      breaks = c(10, 30, 50, 100, 300),
      limits = c(10, 300), show.limits = TRUE
    ) +
    scale_color_viridis_b(
      name = "Median Number of Traits\nacross Species per Region",
      breaks = c(10, 30, 50, 100, 300),
      limits = c(10, 300), show.limits = TRUE
    ) +
    theme_void() +
    theme(
      legend.position  = "top",
      legend.key.width = unit(2, "lines"),
      plot.margin      = margin(b = 3/11)
    )
}

plot_map_sd_n_traits_region = function(
    trait_n_regions, glonaf_small_islands,
    glonaf_mainland_large_islands_simplified
) {

  # Background Map
  world_sf = rnaturalearth::ne_countries(returnclass = "sf") %>%
    sf::st_transform(crs = "+proj=eqearth")

  # Mainland median traits
  mainland_n_traits = glonaf_mainland_large_islands_simplified %>%
    inner_join(trait_n_regions, by = "OBJIDsic")

  # Island median traits
  island_n_traits = glonaf_small_islands %>%
    inner_join(trait_n_regions, by = "OBJIDsic")

  # Clean environment
  rm(trait_n_regions, glonaf_small_islands,
     glonaf_mainland_large_islands_simplified)

  # Actual Plot
  mainland_n_traits %>%
    ggplot(aes(fill = n_traits_sd)) +
    geom_sf(
      data = world_sf |>
        filter(continent != "Antarctica"),
      fill = "gray85", color = "gray65", size = 1/100
    ) +
    # Non-small islands and mainlands
    geom_sf(color = NA, size = 1/100) +
    # Small islands
    geom_sf(
      aes(color = n_traits_sd),
      fill = NA,
      data = island_n_traits,
      size = 2.5, shape = 21, stroke = 1.5, show.legend = FALSE
    ) +
    scale_fill_viridis_b(
      "Standard Deviation of Number of Traits", trans = "log10"
    ) +
    scale_color_viridis_b(
      "Standard Deviation of Number of Traits", trans = "log10"
    ) +
    theme_void() +
    theme(
      legend.position  = "top",
      legend.key.width = unit(2, "lines"),
      plot.margin      = margin(b = 3/11)
    )
}


# Trait Network ----------------------------------------------------------------
plot_network_trait = function(trait_name_network) {

  ggraph::ggraph(trait_name_network, layout = 'graphopt') +
    ggraph::geom_edge_link(
      aes(edge_colour = factor(match_type)), edge_width = 1
    ) +
    ggraph::geom_node_point(aes(shape = database)) +
    labs(
      shape = "Trait Database", edge_colour = "Match type"
    ) +
    theme_void() +
    theme(legend.position = "top")

}


# Other Figures ----------------------------------------------------------------
plot_histogram_number_trait_regions = function(trait_n_regions) {
  trait_n_regions %>%
    select(-match_type) |>
    tidyr::pivot_longer(-OBJIDsic) %>%
    ggplot(aes(value)) +
    geom_histogram(color = "white") +
    facet_wrap(
      vars(name), scales = "free_x",
      labeller = c(
        n_traits_mean   = "Mean # Traits per region",
        n_traits_median = "Median # Traits per region",
        n_traits_sd     = "SD # Traits per region"
      ) %>%
        as_labeller()
    ) +
    scale_x_log10("Number of Traits per Regions") +
    labs(y = "Number of Species") +
    theme_bw() +
    theme(aspect.ratio = 1)
}

plot_data_origin_intersect_top_n_traits = function(
    trait_database_euler_diagrams, n_traits = 20
) {

  # Remove some non-functional traits
  trait_database_euler_diagrams = trait_database_euler_diagrams %>%
    filter(
      !(consolidated_name %in%
          c("Elevational_range_min", "Habitat_1",
            "Species occurrence range: native vs invasive")
        )
    )

  # Needed data
  n_traits = seq(1, n_traits, by = 1)

  # Fill scale (corresponds to Set1)
  db_fills = c(
    "#E69F00",  # (AusTraits) Orange
    "#56B4E9",  # (BIEN)      Blue
    "#009E73",  # (GIFT)      Green
    "#CC79A7"   # (TRY)       Pink
  )

  # Create a horizontal common legend for the whole plot
  common_legend = grid::grid.legend(
    labels = c("AusTraits", "BIEN", "GIFT", "TRY"),
    nrow = 1, ncol = 4,
    pch = 21,
    gp = grid::gpar(
      fill = c(
        "#E69F00",  # (AusTraits) Orange
        "#56B4E9",  # (BIEN)      Blue
        "#009E73",  # (GIFT)      Green
        "#CC79A7"   # (TRY)       Yellow
      ),
      col = "#333333",
      fontsize = 8
    ),
    draw = TRUE
  )

  # Plots
  eulerr::eulerr_options(padding = grid::unit(2/3, "lines"))

  euler_plots = lapply(
    n_traits,
    function(x) {
      plot(
        trait_database_euler_diagrams[["euler"]][[x]],
        # Style the title
        main = list(
          label = ggplot2::label_wrap_gen()(
            trait_database_euler_diagrams[["consolidated_name"]][[x]] %>%
              gsub("_", " ", .) %>%
              tools::toTitleCase()
          ),
          fontsize = 8, cex = 1, lineheight = 0.9, check.overlap = TRUE,
          vjust = 1
        ),
        # Style other elements
        quantities = list(type = "counts", fontsize = 6),
        edges = list(lwd = 0.5, lex = 1, col = "#333333"),
        labels = list(labels = ""),
        fills = list(fill = db_fills)
      )
    }
  )

  # Tidy environment
  rm(trait_database_euler_diagrams)

  # Actual final plot
  patchwork::wrap_plots(
    common_legend,
    patchwork::wrap_plots(list = euler_plots) +
      theme(plot.margin = margin(5.5, 5.5, 5.5, 35)) +
      coord_cartesian(clip = "off"),
    nrow = 2, heights = c(1/20, 19/20)
  )
}

plot_relative_database_importance_traits = function(
    trait_database_euler_diagrams, same_scale_across_facets = TRUE,
    bar_type = c("stack", "fill")
) {

  bar_type = match.arg(bar_type)

  # Get number of observation for each database & intersections
  simplified_df = trait_database_euler_diagrams %>%
    mutate(
      number_table = purrr::map(
        euler, \(x) tibble::enframe(x$original.values, "database", "n_obs")
      ),
      trait_group = (row_number() %/% 15) + 1
    ) %>%
    select(-euler) %>%
    tidyr::unnest(number_table) %>%
    mutate(
      n_inter = stringr::str_count(database, "&"),
      final_name = case_when(
        n_inter == 0 ~ database,
        n_inter != 0 ~ paste0(n_inter + 1, "-DBs intersection")
      )
    ) %>%
    mutate(
      final_name = factor(
        final_name, levels = c(
          "AusTraits", "BIEN", "GIFT", "TRY",
          "2-DBs intersection", "3-DBs intersection", "4-DBs intersection"
        )
      )
    )

  # Count number of times of 4 panels with 15 traits each are necessary
  n_pages = (nrow(trait_database_euler_diagrams) %/% (4 * 15)) + 1

  # Clean environment
  rm(trait_database_euler_diagrams)

  # Actual figure
  lapply(seq(n_pages), function(page_number) {

    simplified_df %>%
      mutate(
        final_name = factor(
          final_name, levels = c(
            "AusTraits", "BIEN", "GIFT", "TRY",
            "2-DBs intersection", "3-DBs intersection", "4-DBs intersection"
          )
        )
      ) %>%
      ggplot(
        aes(n_obs, forcats::fct_reorder(consolidated_name, n_all),
            fill = final_name)
      ) +
      geom_col(position = bar_type) +
      ggforce::facet_wrap_paginate(
        vars(trait_group), nrow = 2, ncol = 2, page = page_number,
        scales = ifelse(same_scale_across_facets, "free_y", "free")
      ) +
      labs(
        x =  ifelse(
          bar_type == "stack",
          "Number of species found in one or more database",
          "Proportion of species found in one or more database"
        ),
        y = "Trait Name",
        fill = "Database"
      ) +
      scale_x_continuous(
        labels = ifelse(bar_type == "fill", scales::label_percent(), identity)
      ) +
      scale_y_discrete(labels = label_wrap_gen(40)) +
      scale_fill_manual(
        values = c(
          AusTraits            = "#E69F00",  # (AusTraits) Orange
          BIEN                 = "#56B4E9",  # (BIEN)      Blue
          GIFT                 = "#009E73",  # (GIFT)      Green
          TRY                  = "#F0E442",  # (TRY)       Yellow
          "2-DBs intersection" = "#AAAAAA",
          "3-DBs intersection" = "#777777",
          "4-DBs intersection" = "#444444"
        ),
        labels = c(
          austraits = "AusTraits", bien = "BIEN", gift = "GIFT", try = "TRY"
        ),
        guide = guide_legend(nrow = 1)
      ) +
      theme_bw() +
      theme(
        legend.position = "top",
        strip.background = element_blank(),
        strip.text = element_blank()
      )

  })


}


plot_venn_diagram_shared_species = function(
    austraits_tnrs, bien_traits_simple, gift_raw_tnrs, try_tnrs, glonaf_tnrs
) {

  # Gather all species from all databases
  all_species = list(
    AusTraits = austraits_tnrs,
    BIEN      = bien_traits_simple |>
      distinct(Accepted_species = scrubbed_species_binomial),
    GIFT      = gift_raw_tnrs,
    TRY       = try_tnrs,
    GloNAF    = glonaf_tnrs
  ) |>
    lapply(
      \(x) x |>
        distinct(Accepted_species) |>
        filter(Accepted_species != "") |>
        pull(Accepted_species)
    )

  plot(
    eulerr::venn(all_species),
    main = "Shared Species Across Databases",
    quantities = list(type = "counts", fontsize = 8),
    edges = list(lwd = 0.5, lex = 1, col = "#333333"),
    fills = list(
      fill = c(
        "#E69F00",  # (AusTraits) Orange
        "#56B4E9",  # (BIEN)      Blue
        "#009E73",  # (GIFT)      Green
        "#CC79A7",  # (TRY)       Pink
        "#C22901"   # (GloNAF)    Red
      )
    )
  )

}
