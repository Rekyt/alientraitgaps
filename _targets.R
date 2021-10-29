# Actual workflow file that describe all analyses

# Initial options --------------------------------------------------------------
library("targets")
library("magrittr")

source("R/bien_functions.R")
source("R/combined_trait_functions.R")
source("R/extract_try_list.R")
source("R/figure_functions.R")
source("R/gbif_functions.R")
source("R/glonaf_db_functions.R")
source("R/harmonize_functions.R")
source("R/invacost_functions.R")
source("R/tr8_functions.R")

tar_option_set(
  packages = c("data.table", "disk.frame", "dplyr", "ggplot2", "here", "TNRS",
               "TR8")
)

# Target factory ---------------------------------------------------------------
list(
  # Load TRY data --------------------------------------------------------------
  # Load actual files
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

  # Match databases against TNRS -----------------------------------------------
  tar_target(
    match_try_tnrs, TNRS::TNRS(try_list)
  ),
  tar_target(
    match_glonaf_tnrs, TNRS::TNRS(glonaf_list)
  ),

  # Harmonize TRY and GloNAF ---------------------------------------------------
  tar_target(
    harmonized_try_glonaf,
    harmonize_try_glonaf_species(match_try_tnrs, match_glonaf_tnrs)
  ),
  # Get back AccSpeciesID after matching
  tar_target(
    harmonized_try_ids,
    get_try_ids_from_harmonized_species(harmonized_try_glonaf, try_species)
  ),

  # Match TRY open data extract
  tar_target(
    try_open_species,
    harmonize_try_open(full_try_df, harmonized_try_glonaf)
  ),

  # Query TRY traits for GloNAF species ----------------------------------------
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
    try_trait_combinations,
    list_trait_combination_per_species(glonaf_try_traits_available)
  ),
  tar_target(
    try_top_traits,
    select_most_measured_traits(glonaf_species_per_trait, 52)
  ),
  tar_target(
    try_trait_combinations_top_traits,
    semi_join(glonaf_try_traits_available, try_top_traits, by = "TraitName") %>%
      list_trait_combination_per_species()
  ),


  # BIEN traits ----------------------------------------------------------------
  # List BIEN traits
  tar_target(
    bien_traits,
    BIEN::BIEN_trait_list()
  ),
  tar_target(
    bien_try_convert_df,
    make_bien_try_correspond(bien_traits)
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
  tar_target(
    bien_trait_combinations,
    list_bien_trait_combination_per_species(glonaf_bien_traits)
  ),


  # Other Trait Data -----------------------------------------------------------
  # Query all possible traits on many databases through TR8
  # tar_target(
  #   glonaf_additional_traits,
  #   get_all_tr8_traits(harmonized_try_glonaf)
  # ),


  # Combine Trait Data ---------------------------------------------------------
  # Combine both trait data from TRY and BIEN
  tar_target(
    consolidated_trait_names,
    consolidate_trait_names(bien_try_convert_df, try_traits)
  ),
  tar_target(
    combined_traits,
    combine_bien_try_traits(consolidated_trait_names, glonaf_bien_traits,
                            glonaf_try_traits_available)
  ),
  # Rank species per trait number in each database
  tar_target(
    glonaf_trait_ranks,
    rank_species_trait_number(
      glonaf_bien_traits_count, try_total_number_trait,
      glonaf_try_traits_available, harmonized_try_glonaf
    )
  ),
  # Count number of tuples among both trait datasets
  trait_combs <- tarchetypes::tar_map(
    # Prepare data frame of parameters
    values = tibble::tibble(
      trait_comb = rlang::syms(
        c("bien_trait_combinations", "try_trait_combinations_top_traits")
      ),
      trait_db = c("bien", "try_open")
    ) %>%
      tidyr::expand_grid(tuple_number = 2:5) %>%
      # Remove cases where trait combinations are too numerous to count
      dplyr::filter(!(trait_db == "try_open" & tuple_number >= 4)),
    names = c("trait_db", "tuple_number"),
    # Actual target
    tar_target(
      trait_comb_number,
      count_tuples_of_traits(
        trait_comb[["trait_names"]], tuple_number, trait_db
      )
    )
  ),
  tarchetypes::tar_combine(
    numbers_trait_combinations,
    trait_combs,
    command = dplyr::bind_rows(!!!.x)
  ),

  tar_target(
    try_diaz_combinations,
    generate_all_possible_diaz_combinations(try_traits)
  ),
  tar_target(
    try_lhs_combinations,
    generate_all_possible_lhs_combinations(try_traits)
  ),

  # See if LHS and Diaz combinations of trait are there
  trait_db_lhs_diaz_count <- tarchetypes::tar_map(
    values = tibble::tibble(
      trait_comb = rlang::syms(
        c("bien_trait_combinations", "try_trait_combinations")
      ),
      db = c("bien", "try_open")
    ),
    names = "db",
    tar_target(
      lhs_diaz_count,
      count_lhs_diaz_combination_trait(trait_comb, try_diaz_combinations,
                                       try_lhs_combinations, db)
    )
  ),
  tarchetypes::tar_combine(
    lhs_diaz_count,
    trait_db_lhs_diaz_count,
    command = dplyr::bind_rows(!!!.x, .id = "db") %>%
      select(-trait_names) %>%
      group_by(db) %>%
      summarise(n_sp          = n(),
                contains_lhs  = sum(contains_lhs),
                contains_diaz = sum(contains_diaz))
  ),


  # InvaCost -------------------------------------------------------------------
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


  # GBIF Occurrences -----------------------------------------------------------
  # Get GBIF ids from species name
  tar_target(
    gbif_ids,
    get_gbif_ids(harmonized_try_glonaf)
  ),
  tar_target(
    gbif_ids_cleaned,
    clean_gbif_ids(gbif_ids)
  ),
  # Get number of GBIF occurrence records
  tar_target(
    gbif_num_occ,
    count_gbif_occurrences(gbif_ids_cleaned)
  ),


  # Make figures ---------------------------------------------------------------
  tar_target(
    fig_glonaf_species_number_trait,
    plot_trait_number_try_glonaf_species(try_total_number_trait)
  ),
  tar_target(
    fig_glonaf_species_per_trait,
    plot_number_species_per_try_trait(
      glonaf_species_per_trait, glonaf_bien_species_per_trait
    )
  ),
  tar_target(
    fig_try_trait_combination_frequency,
    plot_glonaf_trait_combination_frequency(try_trait_combinations, "TRY (open)")
  ),
  tar_target(
    fig_bien_trait_combination_frequency,
    plot_glonaf_trait_combination_frequency(bien_trait_combinations, "BIEN")
  ),
  tar_target(
    fig_trait_ranks_multi_db,
    plot_trait_ranks_multi_db(glonaf_trait_ranks)
  ),
  tar_target(
    fig_species_per_trait_combinations_multi_db,
    plot_species_trait_combinations(numbers_trait_combinations)
  )
) %>%
  # Convert figures into ggplotGrob() to take less space
  tarchetypes::tar_hook_outer(
    ggplot2::ggplotGrob(.x),
    dplyr::starts_with("fig_")
  )
