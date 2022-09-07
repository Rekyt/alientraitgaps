# Workflow file to run all analyses

# Packages and functions -------------------------------------------------------
library("targets")
library("magrittr")

source("R/austraits_functions.R")
source("R/bien_functions.R")
source("R/combined_trait_functions.R")
source("R/figure_functions.R")
source("R/glonaf_functions.R")
source("R/gift_functions.R")
source("R/has_coords_functions.R")
source("R/harmonize_taxonomy_functions.R")
source("R/paper_figures_functions.R")
source("R/try_functions.R")

# Initial options --------------------------------------------------------------
tar_option_set(
  packages = c("data.table", "disk.frame", "dplyr", "ggplot2", "here", "TNRS",
               "TR8", "treemapify", "sf")
)

# Target factory ---------------------------------------------------------------
list(
  # Load TRY data --------------------------------------------------------------
  # Load actual files
  tar_target(
    raw_try_species,
    here::here("inst", "exdata", "try", "TryAccSpecies.txt"),
    format = "file"
  ),
  # Full TRY extract
  tar_target(
    raw_full_try,
    {
      disk.frame::csv_to_disk.frame(
        here::here("inst", "exdata", "try", "17144.txt"),
        here::here("inst", "exdata", "try", "17144.df")
      )
      here::here("inst", "exdata", "try", "17144.df")
    },
    format = "file"
  ),
  tar_target(
    full_try_df,
    disk.frame::disk.frame(raw_full_try)
  ),
  # TRY trait number and trait id file
  tar_target(
    raw_try_traits,
    here::here("inst", "exdata", "try", "try_trait_table_tde20211027142952.txt"),
    format = "file"
  ),
  tar_target(
    try_traits,
    readr::read_delim(raw_try_traits, skip = 3, col_select = -6)
  ),
  # Load species file
  tar_target(
    try_species,
    data.table::fread(raw_try_species, encoding = "UTF-8")
  ),
  # Get the name list
  tar_target(
    try_list, extract_try_list(try_species)
  ),

  # Load GloNAF data -----------------------------------------------------------
  tar_target(
    glonaf_count_alien_status,
    get_glonaf_alien_species_count(connect_glonaf_db())
  ),
  tar_target(
    glonaf_alien_species, get_glonaf_species_list(connect_glonaf_db())
  ),
  tar_target(
    glonaf_list, extract_glonaf_list(glonaf_alien_species)
  ),
  tar_target(
    glonaf_regions_list, get_glonaf_region_correspondence(glonaf_alien_species)
  ),
  tar_target(
    glonaf_regions,
    sf::read_sf(
      here::here("inst", "exdata", "glonaf", "regions_2020-10-28",
                 "regions_2020-10-28.shp")
    )
  ),
  tar_target(
    glonaf_species_number,
    get_glonaf_species_number(connect_glonaf_db())
  ),


  # Load AusTraits data --------------------------------------------------------
  tar_target(
    austraits,
    austraits::load_austraits(version = "3.0.2", path = "inst/exdata/austraits")
  ),
  tar_target(
    austraits_species,
    austraits[["taxa"]]
  ),
  tar_target(
    austraits_list,
    unique(austraits_species[["taxon_name"]])
  ),

  # Load GIFT data -------------------------------------------------------------
  tar_target(
    gift_names,
    read.csv(
      here::here("inst", "exdata", "gift", "GIFT_names_matched.csv"),
      fileEncoding = "utf-8"
    )
  ),
  tar_target(
    gift_traits,
    read.csv(
      here::here("inst", "exdata", "gift", "GIFT_traits_derived.csv"),
      fileEncoding = "utf-8"
    )
  ),
  tar_target(
    gift_traits_final,
    read.csv(
      here::here("inst", "exdata", "gift", "GIFT_traits_final.csv"),
      fileEncoding = "utf-8"
    )
  ),
  tar_target(
    gift_traits_meta,
    read.csv(
      here::here("inst", "exdata", "gift", "GIFT_traits_meta.csv"),
      fileEncoding = "utf-8"
    )
  ),
  tar_target(
    gift_names_traits,
    extract_gift_names_with_traits(gift_traits_final, gift_names)
  ),
  tar_target(
    gift_list,
    extract_gift_species_names(gift_names)
  ),
  tar_target(
    gift_sublist,
    unique(gift_names_traits[["species"]])
  ),

  # TNRS Matching: Harmonize Taxonomies against TNRS ---------------------------
  tar_target(
    match_try_tnrs, TNRS::TNRS(try_list)
  ),
  tar_target(
    match_glonaf_tnrs, TNRS::TNRS(glonaf_list)
  ),
  tar_target(
    match_austraits_tnrs, TNRS::TNRS(austraits_list),
  ),
  tar_target(
    match_gift_tnrs, TNRS::TNRS(gift_sublist)
  ),

  # TRY traits -----------------------------------------------------------------
  # Harmonize TRY and GloNAF
  tar_target(
    harmonized_try_glonaf,
    harmonize_try_glonaf_species(match_try_tnrs, match_glonaf_tnrs)
  ),
  # Get back AccSpeciesID after matching
  tar_target(
    harmonized_try_ids,
    get_try_ids_from_harmonized_species(harmonized_try_glonaf, try_species)
  ),

  tar_target(
    try_total_number_trait, count_trait_try(harmonized_try_glonaf, try_species)
  ),
  tar_target(
    glonaf_try_traits_available,
    list_all_traits_glonaf(harmonized_try_glonaf, try_species, full_try_df)
  ),
  tar_target(
    glonaf_species_per_trait,
    count_species_per_trait(glonaf_try_traits_available)
  ),


  # BIEN traits ----------------------------------------------------------------

  # Query BIEN traits for GloNAF species
  tar_target(
    glonaf_bien_traits_count,
    count_bien_traits_per_species(harmonized_try_glonaf)
  ),
  tar_target(
    glonaf_bien_traits,
    get_bien_traits(harmonized_try_glonaf)
  ),
  tar_target(
    glonaf_bien_species_per_trait,
    count_bien_species_per_trait(glonaf_bien_traits_count)
  ),


  # AusTraits traits -----------------------------------------------------------
  # Match GloNAF to AusTraits
  tar_target(
    harmonized_austraits_glonaf,
    harmonize_austraits_glonaf(match_austraits_tnrs, match_glonaf_tnrs)
  ),

  # Get and count trait data
  tar_target(
    aus_traits,
    get_austraits_traits_for_glonaf_species(
      austraits, harmonized_austraits_glonaf
    )
  ),
  tar_target(
    aus_species_per_trait,
    count_austraits_species_per_trait(aus_traits),
  ),
  tar_target(
    aus_trait_per_species,
    count_austraits_trait_per_species(aus_traits)
  ),


  # GIFT traits ----------------------------------------------------------------
  # Match species names
  tar_target(
    harmonized_gift_glonaf,
    harmonize_gift_glonaf(match_gift_tnrs, match_glonaf_tnrs)
  ),

  # GIFT traits for GloNAF species
  tar_target(
    gift_glonaf_traits,
    get_gift_traits_for_glonaf_species(
      gift_traits_final, gift_names_traits, harmonized_gift_glonaf,
      gift_traits_meta
    )
  ),
  tar_target(
    gift_species_per_trait,
    count_gift_species_per_trait(gift_glonaf_traits),
  ),
  tar_target(
    gift_trait_per_species,
    count_gift_trait_per_species(gift_glonaf_traits)
  ),


  # Consolidate Trait Names ----------------------------------------------------
  tar_target(
    raw_correspondence_tables,
    list.files(
      here::here("inst", "exdata", "correspondence_tables"), full.names = TRUE
    ),
    format = "file"
  ),
  tar_target(
    correspondence_tables, read_correspondence_tables(raw_correspondence_tables)
  ),
  tar_target(
    correspondence_tables_check,
    check_correspondence_tables(
      correspondence_tables, austraits, gift_traits_meta, try_traits
    )
  ),
  tar_target(
    trait_network,
    create_trait_network(
      correspondence_tables_check, austraits, gift_traits_meta, try_traits
    )
  ),
  tar_target(
    network_consolidated_trait_names,
    consolidate_trait_names_from_network(trait_network, try_traits)
  ),


  # Combine Trait Data ---------------------------------------------------------

  ## Actual tables with trait names
  # Actual table with species names and trait names
  tar_target(
    combined_traits,
    combine_bien_try_aus_gift_traits(
      network_consolidated_trait_names, glonaf_bien_traits,
      glonaf_try_traits_available, aus_traits, gift_glonaf_traits
    )
  ),
  # Growth form table per species
  tar_target(
    combined_growth_form,
    extract_growth_form(
      combined_traits, glonaf_bien_traits, gift_traits_final, gift_names_traits,
      harmonized_gift_glonaf, match_glonaf_tnrs
    )
  ),

  # Get higher taxonomy for combined traits
  tar_target(
    combined_traits_taxonomy,
    get_glonaf_higher_taxonomy_combined_traits(
      combined_traits, match_glonaf_tnrs, glonaf_alien_species
    )
  ),

  # Count number of traits per database
  tar_target(
    combined_traits_origin,
    count_traits_per_database(
      network_consolidated_trait_names, glonaf_bien_traits,
      glonaf_try_traits_available, aus_traits, gift_glonaf_traits
    )
  ),

  # Trait Combinations ---------------------------------------------------------
  tar_target(bergmann_comb_df, get_bergmann_combs()),
  tar_target(
    contain_trait_combination,
    count_specific_trait_combinations(
      combined_traits, match_glonaf_tnrs, bergmann_comb_df
    )
  ),


  # Traits with Coordinates ----------------------------------------------------
  tar_target(
    austraits_coords, get_austraits_traits_with_coords(austraits)
  ),
  tar_target(
    bien_traits_coords, get_bien_traits_with_coords(glonaf_bien_traits)
  ),
  tar_target(
    try_traits_coords, get_try_traits_with_coords(full_try_df)
  ),

  # World Regions --------------------------------------------------------------
  tar_target(
    unified_glonaf_regions,
    unify_glonaf_regions(glonaf_regions)
  ),
  tar_target(
    glonaf_small_islands,
    select_glonaf_small_islands(unified_glonaf_regions, area = 2e3)
  ),
  tar_target(
    glonaf_mainland_large_islands,
    select_glonaf_mainland_large_islands(
      unified_glonaf_regions, glonaf_small_islands
    )
  ),
  tar_target(
    glonaf_mainland_large_islands_simplified,
    sf::st_simplify(glonaf_mainland_large_islands, dTolerance = 30000)
  ),
  tar_target(
    glonaf_species_regions_status,
    extract_species_regions_table(connect_glonaf_db(), match_glonaf_tnrs)
  ),
  tar_target(
    glonaf_species_regions,
    distinct(select(glonaf_species_regions_status, -status_name))
  ),
  tar_target(
    regions_trait_prop,
    count_species_proportion_trait_by_region(
      glonaf_species_regions, contain_trait_combination
    )
  ),
  tar_target(
    glonaf_status_trait_cat,
    get_trait_combinations_and_cat_per_invasion_status(
      glonaf_species_regions_status, contain_trait_combination
    )
  ),
  tar_target(
    glonaf_species_area,
    count_number_of_regions_and_area(
      glonaf_species_regions, unified_glonaf_regions
    )
  ),
  tar_target(
    glonaf_most_distributed_species,
    count_most_distributed_species_and_bootstrap(glonaf_species_area, 100, 100)
  ),
  tar_target(
    glonaf_europe,
    get_european_regions_of_glonaf(glonaf_mainland_large_islands)
  ),
  tar_target(
    european_species_traits,
    get_european_glonaf_species(
      glonaf_europe, glonaf_species_regions, contain_trait_combination
    )
  ),
  tar_target(
    trait_n_regions,
    count_number_of_traits_per_region(glonaf_species_regions, combined_traits)
  ),


  # Make figures ---------------------------------------------------------------
  tar_target(
    fig_glonaf_species_number_trait,
    plot_trait_number_try_glonaf_species(try_total_number_trait)
  ),
  tar_target(
    fig_species_per_trait_combined,
    plot_number_species_per_trait_combined(combined_traits)
  ),
  tar_target(
    fig_combined_trait_heatmap,
    plot_combined_traits_heatmap(combined_traits)
  ),
  tar_target(
    fig_number_species_specific_trait_comb,
    plot_number_specific_trait_combination(contain_trait_combination)
  ),
  tar_target(
    fig_trait_combination_taxonomy,
    plot_taxonomy_treemap_trait_combination(
      combined_traits_taxonomy, contain_trait_combination
    )
  ),
  tar_target(
    fig_trait_combination_number_logged,
    plot_taxonomy_treemap_number_traits(
      combined_traits_taxonomy, contain_trait_combination, logged = TRUE
    )
  ),
  tar_target(
    fig_trait_combination_number_unlogged,
    plot_taxonomy_treemap_number_traits(
      combined_traits_taxonomy, contain_trait_combination, logged = FALSE
    )
  ),
  tar_target(
    fig_rich_prop_trait,
    plot_prop_trait_per_richness(regions_trait_prop, unified_glonaf_regions)
  ),
  tar_target(
    fig_rich_ratio_prop_comb,
    plot_proportion_known_combination_per_richness(
      regions_trait_prop, glonaf_species_number
    )
  ),
  tar_target(
    fig_map_glonaf_regions,
    plot_map_glonaf_regions(unified_glonaf_regions)
  ),
  tar_target(
    fig_map_prop_trait_regions,
    plot_map_proportion_trait_by_region(
      regions_trait_prop, glonaf_small_islands,
      glonaf_mainland_large_islands_simplified
    )
  ),
  tar_target(
    fig_map_alien_richness,
    plot_map_alien_richness_region(
      regions_trait_prop, glonaf_small_islands,
      glonaf_mainland_large_islands_simplified
    )
  ),
  tar_target(
    fig_status_prop_comb,
    plot_trait_comb_proportion_per_invasion_status(glonaf_status_trait_cat)
  ),
  tar_target(
    fig_widest_range_trait_comb_prop,
    plot_trait_combination_per_range_size(
      glonaf_species_area, contain_trait_combination
    )
  ),
  tar_target(
    fig_map_europe_trait_prop,
    plot_map_europe_proportion_trait(
      regions_trait_prop, glonaf_small_islands, glonaf_mainland_large_islands
    )
  ),
  tar_target(
    fig_network_trait_name,
    plot_network_trait(trait_network)
  ),
  tar_target(
    fig_trait_histogram_regions,
    plot_histogram_number_trait_regions(trait_n_regions)
  ),
  tar_target(
    fig_map_median_n_traits_region,
    plot_map_median_n_traits_region(
      trait_n_regions, glonaf_small_islands,
      glonaf_mainland_large_islands_simplified
    )
  ),


  # Assembling Figures for Paper -----------------------------------------------
  tar_target(
    pfig1_trait_heatmap_and_freq,
    assemble_fig1(fig_combined_trait_heatmap, fig_species_per_trait_combined)
  ),
  tar_target(
    fig2_treemap_trait_combination,
    fig_trait_combination_taxonomy
  ),
  tar_target(
    pfig3_maps_trait_prop_and_richness,
    assemble_fig3(fig_map_alien_richness, fig_map_prop_trait_regions)
  ),
  tar_target(
    pfig4_trait_comb_prop_status_regions,
    assemble_fig4(fig_status_prop_comb, fig_widest_range_trait_comb_prop)
  )

) %>%
  # Post-processing Hooks ------------------------------------------------------
  # Convert figures into ggplotGrob() to take less space
  tarchetypes::tar_hook_outer(
    ggplot2::ggplotGrob(.x),
    dplyr::starts_with("fig_")
  ) %>%
  # Convert patchwork figures into patchworkGrob()
  tarchetypes::tar_hook_outer(
    patchwork::patchworkGrob(.x),
    dplyr::starts_with("pfig_")
  )
