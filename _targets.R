# Workflow file to run all analyses

# Packages and functions -------------------------------------------------------
library("targets")
library("magrittr")

source("R/apd_functions.R")
source("R/austraits_functions.R")
source("R/bien_functions.R")
source("R/combined_trait_functions.R")
source("R/env_socioeco_vars.R")
source("R/figure_functions.R")
source("R/glonaf_functions.R")
source("R/gift_functions.R")
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

  # Load AusTraits data --------------------------------------------------------
  tar_target(
    austraits,
    austraits::load_austraits(version = "6.0.0", path = "inst/exdata/austraits")
  ),
  tar_target(
    austraits_species,
    austraits[["taxa"]]
  ),


  # Load APD (Australian Plant Traits Dictionary) ------------------------------
  tar_target(
    raw_apd_online,
    {
      loc <- here::here("inst", "exdata", "apd", "apd_flat_traits.csv")
      download.file(
        "https://github.com/traitecoevo/APD/raw/master/data/APD_traits_input.csv",
        loc
      )
      loc
    },
    format = "file",
    cue = tar_cue(mode = "never")
  ),
  tar_target(
    apd_raw, tibble::as_tibble(read.csv(raw_apd_online))
  ),
  tar_target(
    apd_subset, subset_apd(apd_raw)
  ),
  tar_target(
    apd_bien, match_apd_bien(apd_subset)
  ),
  tar_target(
    apd_gift, match_apd_gift(apd_subset)
  ),
  tar_target(
    apd_try, match_apd_try(apd_subset)
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
    glonaf_regions_list, get_glonaf_region_correspondence(glonaf_alien_species)
  ),
  tar_target(
    glonaf_regions,
    sf::read_sf(
      here::here("inst", "exdata", "glonaf", "regions_2023-10-17",
                 "regions_2023_17_10.shp")
    )
  ),


  # Load GIFT data -------------------------------------------------------------
  # Getting GIFT up-to-date version
  tar_target(
    gift_version,
    "3.1"
  ),
  tar_target(
    gift_api,
    Sys.getenv("GIFT_RESTRICTED_API")
  ),
  tar_target(
    gift_species,
    GIFT::GIFT_species(gift_api, GIFT_version = gift_version)
  ),
  tar_target(
    gift_trait_meta,
    GIFT::GIFT_traits_meta(gift_api, GIFT_version = gift_version)
  ),
  tar_target(
    gift_shape,
    GIFT::GIFT_shapes(api = gift_api, GIFT_version = gift_version)
  ),
  tar_target(
    gift_raw_traits,
    GIFT::GIFT_traits_raw(
      gift_trait_meta[["Lvl3"]], api = gift_api,
      GIFT_version = gift_version
    )
  ),
  tar_target(
    gift_checklists,
    retrieve_all_gift_checklists(gift_api, gift_version)
  ),
  tar_target(
    gift_raw_species,
    extract_raw_gift_species(gift_raw_traits, gift_checklists)
  ),
  tar_target(
    gift_raw_list,
    gift_raw_species[["full_name"]]
  ),
  tar_target(
    gift_matched_taxonomy,
    match_taxonomy_checklists_raw(gift_raw_traits, gift_raw_species)
  ),
  tar_target(
    gift_matched_checklists,
    match_checklist(gift_matched_taxonomy, gift_checklists)
  ),


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
    here::here("inst", "exdata", "try", "large_try"),
    format = "file"
  ),
  tar_target(
    full_try_df,
    arrow::read_parquet(raw_full_try)
  ),
  # TRY trait number and trait id file
  tar_target(
    raw_try_traits,
    here::here("inst", "exdata", "try", "tde2024422162351.txt"),
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
    raw_try_harmonized_species,
    here::here(
      "inst", "exdata", "try",
      "Try202442513363549_TRY6.0_SpeciesList_TaxonomicHarmonization.xlsx"
    ),
    format = "file"
  ),
  tar_target(
    try_harmonized_species,
    readxl::read_xlsx(raw_try_harmonized_species)
  ),


  # Harmonizing Taxonomies -----------------------------------------------------
  # Summarise taxonomies for each database
  tar_target(
    austraits_harmonized,
    get_austraits_taxonomy(austraits_species)
  ),
  tar_target(
    bien_harmonized,
    get_bien_taxonomy(bien_species)
  ),
  tar_target(
    gift_harmonized,
    get_gift_taxonomy(gift_species)
  ),
  tar_target(
    try_harmonized,
    get_try_taxonomy(try_harmonized_species)
  ),
  tar_target(
    glonaf_harmonized,
    get_glonaf_taxonomy(glonaf_alien_species)
  ),
  tar_target(
    glonaf_family,
    distinct(glonaf_alien_species, taxon_wcvp_id, taxon_orig_id, taxa_accepted,
             family_wcvp)
  ),
  # Combine each trait taxonomies with GloNAF
  tar_target(
    austraits_glonaf,
    subset_glonaf_species(austraits_harmonized, glonaf_harmonized, "binomial")
  ),
  tar_target(
    bien_glonaf,
    subset_glonaf_species(
      bien_harmonized, glonaf_harmonized, "scrubbed_species_binomial"
    )
  ),
  tar_target(
    gift_glonaf,
    subset_glonaf_species(gift_harmonized, glonaf_harmonized, "work_species")
  ),
  tar_target(
    try_glonaf,
    subset_glonaf_species(try_harmonized, glonaf_harmonized, "MatchedName")
  ),


  # AusTraits traits -----------------------------------------------------------
  # Simplify AusTraits traits
  tar_target(
    austraits_traits_simple,
    simplify_austraits_traits(austraits)
  ),
  # Match GloNAF to AusTraits
  tar_target(
    harmonized_austraits_glonaf,
    harmonize_austraits_glonaf(match_austraits_tnrs, match_glonaf_tnrs)
  ),


  # BIEN traits ----------------------------------------------------------------

  # List of traits in BIEN
  tar_target(
    bien_trait_list, BIEN::BIEN_trait_list()
  ),

  tar_target(
    bien_traits,
    BIEN::BIEN_trait_trait(
      na.omit(bien_trait_list$trait_name), all.taxonomy = TRUE
    )
  ),
  tar_target(
    bien_traits_simple,
    simplify_bien_traits(bien_traits)
  ),
  tar_target(
    bien_species,
    get_bien_taxonomy(bien_traits)
  ),

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


  # GIFT traits ----------------------------------------------------------------
  # Simplify GIFT traits
  tar_target(
    gift_traits_simple,
    simplify_gift_traits(gift_raw_traits)
  ),
  tar_target(
    gift_unified_distribution,
    simplify_gift_distribution(gift_checklists)
  ),

  # TRY traits -----------------------------------------------------------------
  # Simplify
  tar_target(
    try_traits_simple,
    simplify_try_traits(full_try_df)
  ),


  # Consolidate Trait Names ----------------------------------------------------
  tar_target(
    raw_correspondence_tables,
    list.files(
      here::here("inst", "exdata", "correspondence_tables"),
      pattern = ".*correspondence_v2.ods", full.names = TRUE
    ),
    format = "file"
  ),
  tar_target(
    correspondence_tables, read_correspondence_tables(raw_correspondence_tables)
  ),
  tar_target(
    correspondence_tables_check,
    check_correspondence_tables(
      correspondence_tables, bien_trait_list, gift_trait_meta, try_traits,
      apd_bien, apd_gift, apd_try
    )
  ),
  tar_target(
    trait_network,
    create_trait_network(
      correspondence_tables_check, bien_trait_list, gift_trait_meta, try_traits,
      apd_subset, apd_bien, apd_gift, apd_try
    )
  ),
  tar_target(
    trait_network_file,
    write_network_file(
      trait_network, here::here("inst", "trait_network.graphml")
    ),
    format = "file"
  ),
  tarchetypes::tar_map(
    list(match_type = c("full", "close", "exact")),
    tar_target(
      trait_names_nested,
      consolidate_trait_names_from_network(trait_network, match_type)
    ),
    tar_target(
      trait_names,
      unnest_names(trait_names_nested)
    ),

    # Combine Trait Data ---------------------------------------------------------
    tar_target(
      combined_traits,
      combine_traits_all_databases(
        trait_names, austraits_traits_simple, bien_traits_simple,
        gift_traits_simple, try_traits_simple, gift_trait_meta,
        austraits_glonaf, bien_glonaf, gift_glonaf, try_glonaf
      )
    ),
    tar_target(
      simplified_traits, distinct(combined_traits, consolidated_name, species)
    ),
    tar_target(
      database_traits,
      list_species_by_trait_per_database(combined_traits)
    ),
    tar_target(
      database_euler_diagrams,
      intersect_species_list_by_trait_across_database(database_traits)
    ),
    tar_target(
      trait_combinations,
      count_trait_combinations(simplified_traits, match_type, glonaf_harmonized)
    ),
    tar_target(
      fig_euler_diagrams_top_25_traits_database,
      plot_data_origin_intersect_top_n_traits(database_euler_diagrams, 25),
    ),
    tar_target(
      fig_bars_absolute_database_importance_traits,
      plot_relative_database_importance_traits(database_euler_diagrams)
    ),
    tar_target(
      fig_bars_relative_database_importance_traits,
      plot_relative_database_importance_traits(
        database_euler_diagrams, FALSE, "fill"
      )
    )
  ),
  tar_target(
    network_consolidated_trait_names,
    consolidate_trait_names_from_network(trait_network, "full")
  ),
  tar_target(
    standard_growth_form,
    standardize_growth_form(
      trait_names_full, austraits, bien_traits, gift_raw_traits,
      gift_trait_meta, full_try_df, try_harmonized_species
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
      trait_combinations_types, standard_growth_form,
      species_final_socioecovars, glonaf_harmonized
    ),
    pattern = map(trait_combinations_types),
    iteration = "list"
  ),
  tar_target(
    trait_knowledge_model,
    model_alien_trait_knowledge(trait_knowledge_df),
    pattern = map(trait_knowledge_df),
    iteration = "list"
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
      connect_glonaf_db(), glonaf_alien_species
    )
  ),
  tar_target(
    glonaf_species_regions,
    distinct(
      select(glonaf_species_regions_status, -status_name, -taxon_orig_id)
    )
  ),
  tar_target(
    trait_combinations_types,
    list(full = trait_combinations_full, close = trait_combinations_close,
         exact = trait_combinations_exact)
  ),
  tar_target(
    regions_trait_prop,
    count_species_proportion_trait_by_region(
      glonaf_species_regions, trait_combinations_types
    ),
    pattern = map(trait_combinations_types)
  ),
  tar_target(
    glonaf_status_trait_cat,
    get_trait_combinations_and_cat_per_invasion_status(
      glonaf_species_regions_status, trait_combinations_types
    ),
    pattern = map(trait_combinations_types),
    iteration = "list"
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
      glonaf_europe, glonaf_species_regions, trait_combinations_types
    ),
    pattern = map(trait_combinations_types)
  ),
  tar_target(
    combined_trait_types,
    list(full  = combined_traits_full, close = combined_traits_close,
         exact = combined_traits_exact)
  ),
  tar_target(
    trait_n_regions,
    count_number_of_traits_per_region(
      glonaf_species_regions, combined_trait_types
    ),
    pattern = map(combined_trait_types),
    iteration = "list"
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
    fig_species_per_trait_combined,
    plot_number_species_per_trait_combined(
      simplified_traits_full, glonaf_harmonized
    )
  ),
  # Create trait heatmaps with one trait per species = pixels
  # include several variations
  tar_target(
    fig_combined_trait_heatmap,
    plot_combined_traits_heatmap(simplified_traits_full, glonaf_harmonized)
  ),
  tar_target(
    fig_combined_trait_heatmap_200,
    plot_combined_traits_heatmap(
      simplified_traits_full, glonaf_harmonized, 200L
    )
  ),
  tar_target(
    fig_combined_trait_heatmap_zoomed,
    fig_combined_trait_heatmap + ggforce::facet_zoom(xlim = c(1, 200))
  ),
  tar_target(
    fig_combined_trait_heatmap_inset,
    plot_inset_trait_heatmap(
      fig_combined_trait_heatmap_200, fig_combined_trait_heatmap
    )
  ),
  # Display trait combinations (plot & treemaps)
  tar_target(
    fig_number_species_specific_trait_comb,
    plot_number_specific_trait_combination(trait_combinations_full)
  ),
  tar_target(
    fig_treemap_general,
    plot_general_treemap_trait_combination(
      trait_combinations_full, glonaf_harmonized, glonaf_family
    )
  ),
  tar_target(
    fig_trait_combination_taxonomy,
    plot_taxonomy_treemap_trait_combination(
      trait_combinations_full, glonaf_harmonized, glonaf_family
    )
  ),
  tar_target(
    fig_trait_combination_number_logged,
    plot_taxonomy_treemap_number_traits(
      trait_combinations_full, glonaf_harmonized, glonaf_family, logged = TRUE
    )
  ),
  tar_target(
    fig_trait_combination_number_unlogged,
    plot_taxonomy_treemap_number_traits(
      trait_combinations_full, glonaf_harmonized, glonaf_family, logged = FALSE
    )
  ),
  # Proportion of known species per regions
  tar_target(
    fig_map_glonaf_regions,
    plot_map_glonaf_regions(unified_glonaf_regions)
  ),
  tar_target(
    fig_map_prop_trait_regions,
    plot_map_proportion_trait_by_region(
      regions_trait_prop, glonaf_small_islands,
      glonaf_mainland_large_islands_simplified
    ),
    pattern = map(regions_trait_prop),
    iteration = "list"
  ),
  tar_target(
    fig_map_alien_richness,
    plot_map_alien_richness_region(
      regions_trait_prop, glonaf_small_islands,
      glonaf_mainland_large_islands_simplified
    ),
    pattern = map(regions_trait_prop),
    iteration = "list"
  ),
  tar_target(
    fig_status_prop_comb,
    plot_trait_comb_proportion_per_invasion_status(glonaf_status_trait_cat),
    pattern = map(glonaf_status_trait_cat),
    iteration = "list"
  ),
  tar_target(
    fig_widest_range_trait_comb_prop,
    plot_trait_combination_per_range_size(
      glonaf_species_area, trait_combinations_types[[1]]
    ),
    pattern = map(trait_combinations_types),
    iteration = "list"
  ),
  tar_target(
    fig_map_europe_trait_prop,
    plot_map_europe_proportion_trait(
      regions_trait_prop, glonaf_small_islands, glonaf_mainland_large_islands
    ),
    pattern = map(regions_trait_prop),
    iteration = "list"
  ),
  tar_target(
    fig_network_trait_name,
    plot_network_trait(trait_network)
  ),
  tar_target(
    fig_trait_histogram_regions,
    plot_histogram_number_trait_regions(trait_n_regions),
    pattern = map(trait_n_regions),
    iteration = "list"
  ),
  tar_target(
    fig_map_median_n_traits_region,
    plot_map_median_n_traits_region(
      trait_n_regions, glonaf_small_islands,
      glonaf_mainland_large_islands_simplified
    ),
    pattern = map(trait_n_regions),
    iteration = "list"
  ),
  tar_target(
    fig_map_sd_n_traits_region,
    plot_map_sd_n_traits_region(
      trait_n_regions, glonaf_small_islands,
      glonaf_mainland_large_islands_simplified
    ),
    pattern = map(trait_n_regions),
    iteration = "list"
  ),
  tar_target(
    fig_n_traits_n_regions,
    plot_number_of_traits_per_number_of_regions(
      trait_combinations_types[[1]], glonaf_species_area
    ),
    pattern = map(trait_combinations_types),
    iteration = "list"
  ),


  # Assembling Figs. & Models for Paper ----------------------------------------
  tar_target(
    pfig1_trait_heatmap_and_freq,
    assemble_fig1(
      fig_combined_trait_heatmap_inset, fig_species_per_trait_combined
    )
  ),
  tar_target(
    pfig2_treemap_trait_combination,
    assemble_fig2(fig_treemap_general, fig_trait_combination_taxonomy)
  ),
  tar_target(
    pfig3_maps_trait_prop_and_richness,
    assemble_fig3(fig_map_alien_richness[[1]], fig_map_prop_trait_regions[[1]])
  ),
  tar_target(
    pfig4_trait_comb_prop_status_regions,
    assemble_fig4(
      fig_status_prop_comb[[1]], fig_widest_range_trait_comb_prop[[1]]
    )
  ),
  tar_target(
    pfig5_trait_knowledge_model_plot,
    create_fig5(trait_knowledge_model),
    pattern = map(trait_knowledge_model),
    iteration = "list"
  ),
  tar_target(
    supp_fig1_euler_trait_databases,
    fig_euler_diagrams_top_25_traits_database_full
  ),
  tar_target(
    supp_fig2_model_partial_residuals,
    plot_partial_residuals(trait_knowledge_model),
    pattern = map(trait_knowledge_model),
    iteration = "list"
  ),
  tar_target(
    supp_fig3_proportion_species_trait,
    plot_proportion_species_with_trait(
      trait_combinations_types, glonaf_harmonized
    ),
    pattern = map(trait_combinations_types),
    iteration = "list"
  ),
  tar_target(
    supp_fig4_treemap_number_trait,
    plot_treemaps_with_number_of_traits(
      glonaf_family, trait_combinations_full
    )
  ),
  tar_target(
    supp_fig5_maps_median_sd_n_traits,
    assemble_maps_number_of_traits(
      fig_map_median_n_traits_region[[1]], fig_map_sd_n_traits_region[[1]]
    )
  ),


  # Save Plots for paper -------------------------------------------------------
  # Figure 1
  tar_target(
    pfig1_trait_heat_map_and_frequency_pdf,
    ggsave(
      here::here("inst", "figures","figure1_trait_heatmap_and_frequency.pdf"),
      pfig1_trait_heatmap_and_freq, height = 7.5, width = 5.4, units = "in",
      scale = 2
    ),
    format = "file"
  ),
  tar_target(
    pfig1_trait_heat_map_and_frequency_png,
    ggsave(
      here::here("inst", "figures","figure1_trait_heatmap_and_frequency.png"),
      pfig1_trait_heatmap_and_freq, height = 7.5, width = 5.4, units = "in",
      scale = 2, dpi = 300
    ),
    format = "file"
  ),

  # Figure 2
  tar_target(
    pfig2_treemap_trait_combination_pdf,
    ggsave(
      here::here("inst", "figures","figure2_treemap_trait_combination.pdf"),
      pfig2_treemap_trait_combination, height = 7.5, width = 5.4, units = "in",
      scale = 1.2
    ),
    format = "file"
  ),
  tar_target(
    pfig2_treemap_trait_combination_png,
    ggsave(
      here::here("inst", "figures","figure2_treemap_trait_combination.png"),
      pfig2_treemap_trait_combination, height = 7.5, width = 5.4, units = "in",
      scale = 1.2, dpi = 300
    ),
    format = "file"
  ),

  # Figure 3
  tar_target(
    pfig3_maps_trait_prop_and_richness_pdf,
    ggsave(
      here::here("inst", "figures","figure3_maps_trait_prop_and_richness.pdf"),
        pfig3_maps_trait_prop_and_richness, height = 6.2, width = 6.5,
      units = "in", scale = 4/5
    ),
    format = "file"
  ),
  tar_target(
    pfig3_maps_trait_prop_and_richness_png,
    ggsave(
      here::here("inst", "figures","figure3_maps_trait_prop_and_richness.png"),
      pfig3_maps_trait_prop_and_richness, height = 6.2, width = 6.5,
      units = "in", scale = 4/5, dpi = 300
    ),
    format = "file"
  ),

  # Figure 4
  tar_target(
    pfig4_trait_comb_prop_status_regions_pdf,
    ggsave(
      here::here(
        "inst", "figures","figure4_trait_comb_prop_status_regions.pdf"
      ),
      pfig4_trait_comb_prop_status_regions, height = 3.7, width = 6.2,
      units = "in", scale = 1.8
    ),
    format = "file"
  ),
  tar_target(
    pfig4_trait_comb_prop_status_regions_png,
    ggsave(
      here::here(
        "inst", "figures","figure4_trait_comb_prop_status_regions.png"
      ),
      pfig4_trait_comb_prop_status_regions, height = 3.7, width = 6.2,
      units = "in", scale = 1.8, dpi = 300
    ),
    format = "file"
  )
)
