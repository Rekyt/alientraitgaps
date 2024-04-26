# Collect all functions to work with Australian Plant Trait Dictionary (APD)

# Keep only relevant columns with BIEN, GIFT, and TRY
subset_apd = function(raw_apd) {

  raw_apd |>
    select(
      trait_id = identifier, trait, label, contains("BIEN"), contains("GIFT"),
      contains("TRY")
    )

}

# tidying function for easier extraction of matches
# moving from a wide table to a tidy one
tidy_apd = function(apd_subset, dbname = "") {

  apd_subset |>
    filter(if_any(contains(dbname), \(x) x != "")) |>
    select(trait_id, trait, label, contains(dbname)) |>
    tidyr::pivot_longer(
      contains(dbname), names_to = "match_type", values_to = "match_value"
    ) |>
    filter(match_value != "")

}

match_apd_bien = function(apd_subset) {

  apd_subset |>
    tidy_apd("BIEN") |>
    mutate(
      extracted_trait = match_value |>
        stringr::str_extract("([\\w,\\s,;]+)") |>
        stringr::str_split(";") |>
        purrr::map(trimws),
      match_type = stringr::str_extract(match_type, "[:alpha:]+")
    ) |>
    select(-match_value) |>
    tidyr::unnest(extracted_trait)

}

match_apd_gift = function(apd_subset) {

  apd_subset |>
    tidy_apd("GIFT") |>
    mutate(
      extracted_trait = match_value |>
        stringr::str_extract_all("\\[GIFT:[\\d,\\.]+\\]") |>
        purrr::map(stringr::str_remove, "\\[GIFT:") |>
        purrr::map(stringr::str_remove,"\\]"),
      match_type =  stringr::str_extract(match_type, "[:alpha:]+")
    ) |>
    select(-match_value) |>
    tidyr::unnest(extracted_trait) |>
    # Add a level column corresponding to GIFT trait granularity
    mutate(
      gift_lvl = paste0("Lvl", stringr::str_count(extracted_trait, "\\.") + 1)
    )

}

match_apd_try = function(apd_subset) {

  apd_try = apd_subset |>
    tidy_apd("TRY") |>
    mutate(
      extracted_trait = match_value |>
        stringr::str_extract_all("\\[TRY:\\d+\\]") |>
        purrr::map(stringr::str_remove, "\\[TRY:") |>
        purrr::map(stringr::str_remove,"\\]"),
      match_type =  stringr::str_extract(match_type, "[:alpha:]+"),
      # Count number of match traits
      length_extracted = purrr::map_int(extracted_trait, length)
    )

  apd_try_traits_no_problem = apd_try |>
    filter(length_extracted != 0) |>
    select(-length_extracted, -match_value) |>
    tidyr::unnest(extracted_trait)

  apd_try_traits_problem = apd_try |>
    filter(length_extracted == 0) |>
    # Remove traits for which no correspondence is clear
    filter(!(trait %in% c("leaf_lamina_mass_per_area", "life_history"))) |>
    mutate(
      extracted_trait = case_when(
        trait == "cell_cross-sectional_area" ~ lst(c("338", "573")),
        trait == "bark_thickness"            ~ lst(c("24", "3355", "3356")),
        trait == "bark_thickness_index"      ~ lst(c("24", "3355", "3356"))
      )
    ) |>
    select(-length_extracted, -match_value) |>
    tidyr::unnest(extracted_trait)

  bind_rows(apd_try_traits_no_problem, apd_try_traits_problem)

}


