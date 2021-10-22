get_gbif_ids = function(harmonized_try_glonaf) {
  harmonized_try_glonaf %>%
    pull(species_accepted_try) %>%
    unique() %>%
    taxize::get_gbifid_()
}

clean_gbif_ids = function(gbif_ids) {
  gbif_ids %>%
    bind_rows(.id = "submitted_name") %>%
    tidyr::nest(gbif_df = c(usagekey:acceptedusagekey)) %>%
    mutate(
      filtered_gbif = purrr::map(
        gbif_df, ~dplyr::filter(.x, confidence != 0) %>%
          arrange(desc(confidence))
      ),
      n_filtered = purrr::map_int(filtered_gbif, nrow),
      # Get GBIF species keys for matching species and synonyms
      all_species_keys = purrr::map(filtered_gbif, ~.x[["specieskey"]])
    )
}

count_gbif_occurrences = function(gbif_ids_cleaned) {
  gbif_ids_cleaned %>%
    select(submitted_name, all_species_keys) %>%
    mutate(
      all_occ_count = purrr::map(
        all_species_keys,
        # Get occurrence count for each key
        # Consider all kinds of observation from GBIF
        ~purrr::cross(
          .x,
          c("HUMAN_OBSERVATION", "MACHINE_OBSERVATION", "OBSERVATION")
        ) %>%
          purrr::map2_dbl(
            function(x, y) rgbif::occ_count(x, basisOfRecord = y, to = 2021)
          )
      ),
      # Sum all occurrences for each initial species name
      total_occ_count = purrr::map_dbl(all_occ_count, sum)
    )
}
