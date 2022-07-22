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
    geom_point() +
    geom_text(
      aes(label = paste0(round((n_species/total_sp) * 100, 0), "%")),
      hjust = -0.1, size = 2.6, vjust = 0
    ) +
    scale_x_continuous(
      name = "Number of species with traits",
      sec.axis = sec_axis(
        trans = ~.x/total_sp, labels = scales::percent_format()
      )
    ) +
    scale_y_discrete(
      name = "Trait name",
      labels = c(
        life_form                    = "Growth Form (cat.)",
        woodiness                    = "Woodiness (cat.)",
        life_history                 = "Life History (cat.)",
        leaf_type                    = "Leaf Type (cat.)",
        plant_height                 = "Plant Height (cont.)",
        leaf_compoundness            = "Leaf Compoundness (cat.)",
        photosynthetic_pathway       = "Photosynthetic Pathway (cat.)",
        diaspore_mass                = "Seed Mass (cont.)",
        `Leaflet number per leaf`    = "Leaflet number per leaf (cont.)",
        dispersal_appendage          = "Fruit Type (cat.)",
        leaf_phenology               = "Leaf Phenology (cat.)",
        `Species tolerance to frost` = "Species tolerane to frost (cont.)",
        flowering_time               = "Flowering Phenology (cat.)",
        dispersal_syndrome           = "Fruit Dispersal Syndrome (cat.)",
        nitrogen_fixing              = "Nitrogen Fixer (bin.)",
        leaf_length                  = "Leaf Length (cont.)",
        leaf_width                   = "Leaf Width (cont.)",
        pollination_syndrome         = "Pollination Syndrome (cat.)",
        leaf_arrangement             = "Leaf Arrangement (cat.)",
        sex_type                     = "Flower Sex Syndrome (cat.)"
      )
    ) +
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
                             "has_at_least_one_trait")
      )
    ) +
    geom_vline(
      xintercept = nrow(contain_trait_combination), linetype = 2, size = 1,
      color = "darkred"
    ) +
    geom_point(size = 2, color = "darkblue") +
    geom_text(
      aes(
        label = paste0(
          round(n_species/nrow(contain_trait_combination) * 100, 1), "%"
        )
      ), hjust = -0.2, vjust = 0.5
    ) +
    scale_x_continuous(
      sec.axis = sec_axis(
        ~./nrow(contain_trait_combination), labels = scales::percent_format()
      )
    ) +
    scale_y_discrete(
      labels = c(
        in_glonaf = "In GloNAF",
        has_at_least_one_trait = "At least one trait",
        has_lhs   = "Leaf-Height-Seed\n(3 traits, Westoby 1998)",
        has_diaz  = "Aboveground traits\n(6 traits, Díaz et al., 2016)",
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
  trait_comb_prop_status = glonaf_status_trait_cat %>%
    group_by(status_name) %>%
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
    select(status_name, n) %>%
    mutate(
      labels = paste0(status_name, "\n(n = ", format(n, big.mark = ","), ")")
    ) %>%
    select(-n) %>%
    tibble::deframe()

  trait_comb_prop_status %>%
    select(status_name, ends_with("prop")) %>%
    tidyr::pivot_longer(
      ends_with("prop"), names_to = "comb_name", values_to = "comb_value"
    ) %>%
    mutate(
      status_name = factor(
        status_name, levels = c("alien", "naturalized", "invasive")
      ),
      comb_name = factor(
        comb_name,
        levels = c("has_bergmann_prop", "has_diaz_prop", "has_lhs_prop",
                   "has_at_least_one_trait_prop")
      )
    ) %>%
    ggplot(aes(comb_name, comb_value, shape = status_name, color = status_name)) +
    geom_point(position = position_dodge(width = 0.5), size = 2) +
    geom_linerange(
      aes(x = comb_name, ymin = 0, ymax = comb_value,
          group = interaction(status_name, comb_name)),
      position = position_dodge(width = 0.5)
    ) +
    scale_x_discrete(
      labels = c(
        has_bergmann_prop           = "Root traits",
        has_diaz_prop               = "Aboveground traits",
        has_lhs_prop                = "LHS traits",
        has_at_least_one_trait_prop = "At least one trait"
      )
    ) +
    scale_y_continuous(
      "Proportion of Species", labels = scales::label_percent(), limits = c(0, 1)
    ) +
    scale_shape_discrete(
      "Species Status", guide = guide_legend(reverse = TRUE),
      labels = status_labels
    ) +
    scale_color_manual("Species Status", values = c(
      invasive = "#E69F00", naturalized = "#56B4E9", alien = "#009E73"
    ), guide = guide_legend(reverse = TRUE), labels = status_labels
    ) +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "top", panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank())
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
      aes(
        area = 1,
        fill = interaction(
          has_at_least_one_trait, has_lhs, has_diaz, has_bergmann
        ),
        label = genus, subgroup = family,
        subgroup2 =  interaction(
          has_at_least_one_trait, has_lhs, has_diaz, has_bergmann
        )
      )
    ) +
    treemapify::geom_treemap(color = NA, layout = "scol") +
    treemapify::geom_treemap_subgroup_border(
      size = 2, color = "white", layout = "scol"
    ) +
    treemapify::geom_treemap_subgroup2_border(
      size = 1/3, color = "white", layout = "scol"
    ) +
    treemapify::geom_treemap_subgroup_text(
      place = "centre", grow = TRUE, alpha = 2/3, colour = "black",
      fontface = "italic", min.size = 7, layout = "scol"
    ) +
    scale_fill_manual(
      name = "Trait combination",
      labels = c(
        "FALSE.FALSE.FALSE.FALSE" = "No trait",
        "TRUE.FALSE.FALSE.FALSE"  = "At least\none trait",
        "TRUE.FALSE.FALSE.TRUE"   = "Root traits\n(Bergmann et al. 2022)",
        "TRUE.TRUE.FALSE.FALSE"   = "Leaf-Height-Seed\n(Westoby 1998)",
        "TRUE.TRUE.FALSE.TRUE"    = "LHS and Root traits",
        "TRUE.TRUE.TRUE.FALSE"    = "Aboveground traits\n(Díaz et al., 2016)",
        "TRUE.TRUE.TRUE.TRUE"     = "Aboveground and Root traits"
      ),
      values = c(
        "FALSE.FALSE.FALSE.FALSE" = "white",    # No trait
        "TRUE.FALSE.FALSE.FALSE"  = "#d3d3d3",  # >=1 trait(s)
        "TRUE.FALSE.FALSE.TRUE"   = "#d25601",  # Root traits
        "TRUE.TRUE.FALSE.FALSE"   = "#9283ac",  # LHS
        "TRUE.TRUE.FALSE.TRUE"    = "#923601",  # LHS + Root
        "TRUE.TRUE.TRUE.FALSE"    = "#563787",  # Aboveground
        "TRUE.TRUE.TRUE.TRUE"     = "#551601"   # Aboveground + Root
      )
    ) +
    theme(legend.position = "top", aspect.ratio = 1)
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
      x    = "Trait Rank",
      y    = "Species Rank",
      fill = "Was trait measured?"
    ) +
    scale_fill_viridis_d(labels = c(`TRUE` = "Yes", `FALSE` = "No")) +
    coord_cartesian(expand = FALSE) +
    theme_bw() +
    theme(legend.position = "top")
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
    geom_sf(color = NA, size = 1/100) +
    # Small islands
    geom_sf(
      aes(color = prop_value),
      fill = NA,
      data = glonaf_small_islands %>%
        inner_join(pivoted_data, by = "OBJIDsic"),
      size = 1.2, shape = 21, stroke = 0.4
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
    scale_fill_viridis_b(
      name = "Prop. of aliens species\nwith trait combination",
      labels = scales::percent_format(), n.breaks = 6, show.limits = TRUE
    ) +
    scale_color_viridis_b(
      name = "Prop. of aliens species\nwith trait combination",
      labels = scales::percent_format(), n.breaks = 6, show.limits = TRUE
    ) +
    ylim(-5747986, NA) +  # Remove whatever is below 60°S
    theme_void() +
    theme(
      legend.position = "top",
      legend.key.width = unit(2, "lines"),
      strip.background = element_blank(),
      plot.margin      = margin(b = 3/11)
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
    theme(
      legend.position  = "top",
      legend.key.width = unit(2, "lines"),
      plot.margin      = margin(b = 3, unit = "pt")
    )
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
          has_bergmann_prop       = paste0("Root Traits\n",
                                           "(4 traits, Bergmann et al. 2020)"),
          has_diaz_prop           = paste0("Aboveground traits\n",
                                           "(6 traits, Díaz et al. 2016)"),
          has_lhs_prop            = paste0("Leaf-Height-Seed mass\n",
                                           "(3 traits, Westoby 2002)"),
          prop_with_any_trait     = "Any trait",
          flower_prop_trait       = "Prop. Aliens\nFlower",
          height_prop_trait       = "Prop. Aliens\nHeight",
          leaf_prop_trait         = "Prop. Aliens\nLeaf",
          life_history_prop_trait = "Prop. Aliens\nLife History",
          root_prop_trait         = "Prop. Aliens\nRoot",
          seed_prop_trait         = "Prop. Aliens\nSeed",
          stem_prop_trait         = "Prop. Aliens\nStem"
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


# Trait Network ----------------------------------------------------------------
plot_network_trait = function(trait_name_network) {

  ggraph::ggraph(trait_name_network, layout = 'graphopt') +
    ggraph::geom_edge_link(aes(colour = identical), edge_width = 1) +
    ggraph::geom_node_point(aes(shape = database)) +
    labs(
      shape = "Trait Database", edge_colour = "Are Traits Identical or Similar?"
    ) +
    ggraph::scale_edge_color_discrete(
      labels = c(yes = "identical", no = "similar")
    ) +
    theme_void() +
    theme(legend.position = "top")

}
