# Workflow file to run all analyses

# Packages and functions -------------------------------------------------------
library("targets")
library("magrittr")

source("R/austraits_functions.R")
source("R/bien_functions.R")
source("R/combined_trait_functions.R")
source("R/env_socioeco_vars.R")
source("R/figure_functions.R")
source("R/glonaf_functions.R")
source("R/gift_functions.R")
source("R/has_coords_functions.R")
source("R/harmonize_taxonomy_functions.R")
source("R/paper_figures_functions.R")
source("R/trait_knowledge.R")
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


  # TNRS Matching: Harmonize Taxonomies against TNRS ---------------------------
  tar_target(
    match_try_tnrs, get_tnrs_values(try_list, "try")
  ),
  tar_target(
    match_glonaf_tnrs, get_tnrs_values(glonaf_list, "glonaf")
  ),
  tar_target(
    match_austraits_tnrs, get_tnrs_values(austraits_list, "austraits"),
  ),
  tar_target(
    match_raw_gift_tnrs, get_tnrs_values(gift_raw_list, "gift-raw")
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
    harmonize_gift_glonaf(match_raw_gift_tnrs, match_glonaf_tnrs)
  ),

  # GIFT traits for GloNAF species
  tar_target(
    gift_glonaf_traits,
    get_gift_traits_for_glonaf_species(
      gift_all_raw_traits, gift_matched_taxonomy, harmonized_gift_glonaf,
      gift_current_trait_meta
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
  # Getting GIFT up-to-date
  tar_target(
    gift_version,
    "2.2"
  ),
  tar_target(
    gift_api,
    Sys.getenv("GIFT_RESTRICTED_API")
  ),
  tar_target(
    gift_all_species,
    GIFT::GIFT_species(gift_api, GIFT_version = gift_version)
  ),
  tar_target(
    gift_current_trait_meta,
    GIFT::GIFT_traits_meta(gift_api, GIFT_version = gift_version)
  ),
  tar_target(
    gift_shape,
    GIFT::GIFT_shapes(api = gift_api, GIFT_version = gift_version)
  ),
  tar_target(
    gift_all_raw_traits,
    GIFT::GIFT_traits_raw(
      gift_current_trait_meta[["Lvl3"]], api = gift_api,
      GIFT_version = gift_version
    )
  ),
  tar_target(
    gift_checklists,
    retrieve_all_gift_checklists(gift_api, gift_version)
  ),
  tar_target(
    gift_raw_species,
    extract_raw_gift_species(gift_all_raw_traits, gift_checklists)
  ),
  tar_target(
    gift_raw_list,
    gift_raw_species[["full_name"]]
  ),
  tar_target(
    gift_matched_taxonomy,
    match_taxonomy_checklists_raw(
      gift_all_raw_traits, gift_raw_species, gift_raw_list, match_raw_gift_tnrs
    )
  ),
  tar_target(
    gift_matched_checklists,
    match_checklist(gift_matched_taxonomy, gift_checklists)
  ),
  tar_target(
    gift_unified_distribution,
    simplify_gift_distribution(gift_matched_checklists)
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
      correspondence_tables, austraits, gift_current_trait_meta, try_traits
    )
  ),
  tar_target(
    trait_network,
    create_trait_network(
      correspondence_tables_check, austraits, gift_current_trait_meta, try_traits
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
      combined_traits, glonaf_bien_traits, gift_all_raw_traits,
      gift_matched_taxonomy, harmonized_gift_glonaf, match_glonaf_tnrs,
      glonaf_list
    )
  ),
  tar_target(
    simplified_growth_form,
    simplify_growth_form(combined_growth_form)
  ),

  # Get higher taxonomy for combined traits
  tar_target(
    combined_traits_taxonomy,
    get_glonaf_higher_taxonomy_combined_traits(match_glonaf_tnrs)
  ),

  # Count number of traits per database
  tar_target(
    combined_traits_origin,
    count_traits_per_database(
      network_consolidated_trait_names, glonaf_bien_traits,
      glonaf_try_traits_available, aus_traits, gift_glonaf_traits
    )
  ),
  tar_target(
    trait_database_sp_list,
    list_species_by_trait_per_database(combined_traits_origin)
  ),
  tar_target(
    trait_database_euler_diagrams,
    intersect_species_list_by_trait_across_database(trait_database_sp_list)
  ),

  # Trait Combinations ---------------------------------------------------------
  tar_target(bergmann_comb_df, get_bergmann_combs()),
  tar_target(
    contain_trait_combination,
    count_specific_trait_combinations(
      combined_traits, match_glonaf_tnrs, bergmann_comb_df
    )
  ),


  # Trait Knowledge Model ------------------------------------------------------
  tar_target(
    alien_range_size,
    get_alien_range_size(glonaf_species_regions_status)
  ),
  tar_target(
    invasive_range_size,
    get_invasive_range_size(glonaf_species_regions_status)
  ),
  tar_target(
    trait_knowledge_df,
    assemble_trait_knowledge_df(
      combined_traits, simplified_growth_form, species_final_socioecovars,
      match_glonaf_tnrs
    )
  ),
  tar_target(
    trait_knowledge_model,
    model_alien_trait_knowledge(trait_knowledge_df)
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
    extract_species_regions_table(
      connect_glonaf_db(), match_glonaf_tnrs, glonaf_list
    )
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

  # Get environmental & socioeconomic variables --------------------------------
  tar_target(
    world_socioeco,
    get_socioeco_variables()
  ),
  tar_target(
    avg_socioeco,
    average_country_socioeco_vars(world_socioeco)
  ),
  tar_target(
    road_density_file,
    here::here("inst", "exdata", "grip", "grip4_total_dens_m_km2.asc"),
    format = "file"
  ),
  tar_target(
    population_count_file,
    here::here(
      "inst", "exdata", "gpw", "gpw_v4_population_count_adjusted_rev11_1_deg.nc"
    )
  ),
  tar_target(
    population_density_file,
    here::here(
      "inst", "exdata", "gpw",
      "gpw_v4_population_density_adjusted_rev11_1_deg.nc"
    )
  ),
  tar_target(
    glonaf_road_density,
    extract_road_density_glonaf_regions(
      glonaf_regions, road_density_file
    )
  ),
  tar_target(
    glonaf_gdp_research,
    get_world_bank_indicators_on_glonaf(
      avg_socioeco, glonaf_regions
    )
  ),
  tar_target(
    glonaf_pop_count,
    extract_pop_count_glonaf_regions(
      glonaf_regions, population_count_file
    )
  ),
  tar_target(
    glonaf_pop_density,
    extract_pop_density_glonaf_regions(
      glonaf_regions, population_density_file
    )
  ),
  tar_target(
    species_socioecovars,
    combine_species_socioecovars(
      glonaf_species_regions, glonaf_road_density, glonaf_pop_density,
      glonaf_pop_count, glonaf_gdp_research
    )
  ),
  tar_target(
    gift_socioecovars,
    get_gift_socioecovars(gift_api, gift_version)
  ),
  tar_target(
    species_gift_count_socioecovars,
    count_socioecovars_cover(gift_socioecovars, gift_unified_distribution)
  ),
  tar_target(
    species_gift_socioecovars,
    compute_gift_species_socioecovars(
      gift_socioecovars, gift_unified_distribution
    )
  ),
  tar_target(
    species_final_socioecovars,
    combine_and_filter_socioecovars(
      species_socioecovars, species_gift_count_socioecovars,
      species_gift_socioecovars
    )
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
    plot_combined_traits_heatmap(combined_traits, match_glonaf_tnrs)
  ),
  tar_target(
    fig_number_species_specific_trait_comb,
    plot_number_specific_trait_combination(contain_trait_combination)
  ),
  tar_target(
    fig_treemap_general,
    plot_general_treemap_trait_combination(
      combined_traits_taxonomy, contain_trait_combination
    )
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
  tar_target(
    fig_map_sd_n_traits_region,
    plot_map_sd_n_traits_region(
      trait_n_regions, glonaf_small_islands,
      glonaf_mainland_large_islands_simplified
    )
  ),
  tar_target(
    fig_n_traits_n_regions,
    plot_number_of_traits_per_number_of_regions(
      contain_trait_combination, glonaf_species_area
    )
  ),
  tar_target(
    fig_euler_diagrams_top_20_traits_database,
    plot_data_origin_intersect_top_20_traits(trait_database_euler_diagrams),
  ),
  tar_target(
    fig_bars_absolute_database_importance_traits,
    plot_relative_database_importance_traits(trait_database_euler_diagrams)
  ),
  tar_target(
    fig_bars_relative_database_importance_traits,
    plot_relative_database_importance_traits(
      trait_database_euler_diagrams, FALSE, "fill"
    )
  ),


  # Assembling Figs. & Models for Paper ----------------------------------------
  tar_target(
    pfig1_trait_heatmap_and_freq,
    assemble_fig1(fig_combined_trait_heatmap, fig_species_per_trait_combined)
  ),
  tar_target(
    pfig2_treemap_trait_combination,
    assemble_fig2(fig_treemap_general, fig_trait_combination_taxonomy)
  ),
  tar_target(
    pfig3_maps_trait_prop_and_richness,
    assemble_fig3(fig_map_alien_richness, fig_map_prop_trait_regions)
  ),
  tar_target(
    pfig4_trait_comb_prop_status_regions,
    assemble_fig4(fig_status_prop_comb, fig_widest_range_trait_comb_prop)
  ),
  tar_target(
    ptab2_trait_knowledge_model_summary,
    create_trait_knowledge_table(trait_knowledge_model)
  ),
  tar_target(
    supp_fig1_model_partial_residuals,
    plot_partial_residuals(trait_knowledge_model)
  ),
  tar_target(
    supp_fig2_proportion_species_trait,
    plot_proportion_species_with_trait(combined_traits, match_glonaf_tnrs)
  ),
  tar_target(
    supp_fig3_treemap_number_trait,
    plot_treemaps_with_number_of_traits(
      combined_traits_taxonomy, contain_trait_combination
    )
  ),
  tar_target(
    supp_fig4_maps_median_sd_n_traits,
    assemble_maps_number_of_traits(
      fig_map_median_n_traits_region, fig_map_sd_n_traits_region
    )
  )
)
