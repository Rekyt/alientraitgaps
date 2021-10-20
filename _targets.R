# Actual workflow file that describe all analyses

# Initial options --------------------------------------------------------------
library("targets")
library("magrittr")

source("R/bien_functions.R")
source("R/extract_try_list.R")
source("R/figure_functions.R")
source("R/glonaf_db_functions.R")
source("R/harmonize_try_glonaf.R")
source("R/invacost_functions.R")

tar_option_set(
  packages = c("data.table", "disk.frame", "dplyr", "ggplot2", "here", "TNRS")
)

# Target factory ---------------------------------------------------------------
list(
  # Load TRY data
  tar_target(
    raw_try_df,
    here::here("inst", "exdata", "try", "12477.df"),
    format = "file"
  ),
  tar_target(
    try_df,
    disk.frame::disk.frame(raw_try_df)
  ),
  tar_target(
    raw_try_species,
    here::here("inst", "exdata", "try", "TryAccSpecies.txt"),
    format = "file"
  ),
  tar_target(
    try_species,
    data.table::fread(raw_try_species, encoding = "UTF-8")
  ),
  tar_target(
    try_list, extract_try_list(try_species)
  ),

  # Load GloNAF data
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

  # Match databases against TNRS
  tar_target(
    match_try_tnrs, TNRS::TNRS(try_list)
  ),
  tar_target(
    match_glonaf_tnrs, TNRS::TNRS(glonaf_list)
  ),

  # Harmonize TRY and GloNAF
  tar_target(
    harmonized_try_glonaf,
    harmonize_try_glonaf_species(match_try_tnrs, match_glonaf_tnrs)
  ),

  # Query TRY traits for GloNAF species
  tar_target(
    try_total_number_trait, count_trait_try(harmonized_try_glonaf, try_species)
  ),
  tar_target(
    glonaf_try_traits_available,
    list_all_traits_glonaf(harmonized_try_glonaf, try_species, try_df)
  ),
  tar_target(
    glonaf_species_per_trait,
    count_species_per_trait(glonaf_try_traits_available)
  ),
  tar_target(
    try_trait_combination,
    list_trait_combination_per_species(glonaf_try_traits_available)
  ),

  # Get InvaCost data
  tar_target(
    invacost_data,
    download_invacost(),
    format = "file"
  ),
  tar_target(
    invacost_files,
    unzip_invacost(invacost_data),
    format = "file"
  ),

  # Match InvaCost data

  # Query BIEN traits for GloNAF species
  tar_target(
    glonaf_bien_traits_count,
    count_bien_traits_per_species(harmonized_try_glonaf)
  ),
  tar_target(
    glonaf_bien_traits,
    get_bien_traits(harmonized_try_glonaf)
  ),

  # Make figures
  tar_target(
    fig_glonaf_species_number_trait,
    plot_trait_number_try_glonaf_species(try_total_number_trait)
  ),
  tar_target(
    fig_glonaf_species_per_trait,
    plot_number_species_per_try_trait(glonaf_species_per_trait)
  ),
  tar_target(
    fig_trait_combination_frequency,
    plot_glonaf_try_trait_combination_frequency(try_trait_combination)
  )


) %>%
  tarchetypes::tar_hook_outer(
    ggplot2::ggplotGrob(.x),
    dplyr::starts_with("fig_")
  )
