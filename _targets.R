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
source("R/harmonize_taxonomy_functions.R")
source("R/try_functions.R")

# Initial options --------------------------------------------------------------
tar_option_set(
  packages = c("data.table", "disk.frame", "dplyr", "ggplot2", "here", "TNRS",
               "TR8", "treemapify")
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
  tar_target(
    raw_austraits_try_convert_table,
    here("inst", "exdata", "austraits", "AusTraits-TRY matches.xlsx"),
    format = "file"
  ),
  tar_target(
    austraits_try_convert,
    readxl::read_xlsx(raw_austraits_try_convert_table, na = c("", "NA"))
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
  tar_target(
    try_trait_categories,
    make_try_trait_categories(consolidated_trait_names)
  ),


  # BIEN traits ----------------------------------------------------------------
  # List BIEN traits
  tar_target(
    bien_trait_list,
    BIEN::BIEN_trait_list()
  ),
  tar_target(
    bien_try_convert_df,
    make_bien_try_correspond(bien_trait_list)
  ),
  tar_target(
    bien_trait_categories,
    make_bien_trait_category(bien_trait_list)
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


  # AusTraits traits -----------------------------------------------------------
  # Match GloNAF to AusTraits
  tar_target(
    harmonized_austraits_glonaf,
    harmonize_austraits_glonaf(match_austraits_tnrs, match_glonaf_tnrs)
  ),

  # Make correspondance between AusTraits and TRY
  tar_target(
    aus_try_convert_df,
    make_austraits_try_traits_correspond(austraits_try_convert)
  ),

  # Make correspondance between AusTraits and BIEN (for the one not in TRY)
  tar_target(
      aus_bien_convert_df,
    make_austraits_bien_traits_correspond(
      aus_try_convert_df, bien_try_convert_df
    )
  ),
  # Non-TRY traits categories
  tar_target(
    aus_trait_categories,
    make_non_try_aus_traits_category(consolidated_trait_names)
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
  # Trait conversion table
  tar_target(
    gift_try_convert_df,
    make_gift_try_traits_correspond(gift_traits_meta)
  ),
  tar_target(
    gift_trait_categories,
    make_gift_trait_category(gift_traits_meta)
  ),

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


  # Combine Trait Data ---------------------------------------------------------
  # Combine trait names from BIEN, AusTraits, and TRY under a common umbrella
  tar_target(
    consolidated_trait_names,
    consolidate_trait_names(
      bien_try_convert_df, aus_try_convert_df, aus_bien_convert_df,
      gift_try_convert_df, try_traits
    )
  ),
  # Add trait categories to traits
  tar_target(
    consolidated_trait_categories,
    combine_trait_categories(
      consolidated_trait_names, gift_trait_categories, aus_trait_categories,
      bien_trait_categories, try_trait_categories
    )
  ),
  ## Actual tables with trait names
  # Actual table with species names and trait names
  tar_target(
    combined_traits,
    combine_bien_try_aus_gift_traits(
      consolidated_trait_names, glonaf_bien_traits, glonaf_try_traits_available,
      aus_traits, gift_glonaf_traits
    )
  ),
  # Add trait categories
  tar_target(
    combined_trait_categories_species,
    inner_join(combined_traits, consolidated_trait_categories,
               by = "consolidated_name")
  ),
  # Growth form table per species
  tar_target(
    combined_growth_form,
    extract_growth_form(
      combined_traits, glonaf_bien_traits, gift_traits_final, gift_names_traits,
      harmonized_gift_glonaf, match_glonaf_tnrs
    )
  ),

  # Rank species per trait number in each database
  tar_target(
    glonaf_trait_ranks,
    rank_species_trait_number(
      glonaf_bien_traits_count, try_total_number_trait,
      glonaf_try_traits_available, harmonized_try_glonaf
    )
  ),

  # Get higher taxonomy for combined traits
  tar_target(
    combined_traits_taxonomy,
    get_glonaf_higher_taxonomy_combined_traits(
      combined_traits, match_glonaf_tnrs, glonaf_alien_species
    )
  ),
  # Get for each species the number of traits in each category
  tar_target(
    species_trait_categories,
    count_trait_categories_per_species(
      combined_trait_categories_species, match_glonaf_tnrs
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


  # World Regions --------------------------------------------------------------
  tar_target(
    unified_glonaf_regions,
    unify_glonaf_regions(glonaf_regions)
  ),
  tar_target(
    glonaf_species_regions,
    extract_species_regions_table(connect_glonaf_db(), match_glonaf_tnrs)
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
    fig_miss_trait_cat_species,
    plot_miss_trait_categories_per_species(species_trait_categories)
  ),
  tar_target(
    pfig_miss_trait_cat_species_growth_form,
    plot_miss_trait_categories_per_species_per_growth_form(
      species_trait_categories, combined_growth_form
    )
  ),
  tar_target(
    fig_miss_trait_cat_species_summary,
    plot_miss_trait_categories_per_species_summary(species_trait_categories)
  ),
  tar_target(
    fig_count_trait_cat_species,
    plot_number_trait_categories_per_species(species_trait_categories)
  ),
  tar_target(
    fig_map_glonaf_regions,
    plot_map_glonaf_regions(unified_glonaf_regions)
  )
) %>%
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
