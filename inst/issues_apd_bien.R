# Investigating correspondence between APD and BIEN
library("dplyr")
library("targets")

tar_load(apd_subset)
tar_load(bien_trait_list)

apd_bien_detailed = apd_subset |>
  select(trait_id:label, starts_with("BIEN")) |>
  tidyr::pivot_longer(
    starts_with("BIEN"), names_to = "match_type", values_to = "matched_trait"
  ) |>
  filter(matched_trait != "") |>
  mutate(
    # Split for traits that have multiple matches on one line
    split_traits = purrr::map(stringr::str_split(matched_trait, ";"), trimws),
    # Extract GIFT trait name
    extracted_trait = purrr::map(
      split_traits, \(x) stringr::str_extract(x, "^(.*)\\s\\[", group = 1)
    )
  ) |>
  # Put everything in a tidy format
  tidyr::unnest(split_traits:extracted_trait)

## Potentially problematic traits
apd_bien_detailed |>
  filter(!(extracted_trait %in% na.omit(bien_trait_list[["trait_name"]])))
