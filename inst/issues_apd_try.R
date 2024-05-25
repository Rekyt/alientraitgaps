# Investigating correspondence between APD and TRY
library("dplyr")
library("targets")

tar_load(apd_subset)
tar_load(try_traits)

apd_try_detailed = apd_subset |>
  select(trait_id:label, starts_with("TRY")) |>
  tidyr::pivot_longer(
    starts_with("TRY"), names_to = "match_type", values_to = "matched_trait"
  ) |>
  filter(matched_trait != "") |>
  mutate(
    # Split for traits that have multiple matches on one line
    split_traits = purrr::map(stringr::str_split(matched_trait, ";"), trimws),
    # Extract GIFT trait name
    extracted_trait = purrr::map(
      split_traits, \(x) stringr::str_extract(x, "^(.*)\\s\\[", group = 1)
    ),
    # Extract GIFT trait code
    extracted_code = purrr::map(
      split_traits, \(x) stringr::str_extract(x, "\\[TRY:(.+)\\]", group = 1) |>
        as.numeric()
    )
  ) |>
  tidyr::unnest(split_traits:extracted_code)

apd_try_smaller = apd_try_detailed |>
  # Match names based on trait code
  left_join(
    try_traits |>
      distinct(TraitID, name_matched_on_code = Trait),
    by = c(extracted_code = "TraitID")
  ) |>
  # Match code based on trait name
  left_join(
    try_traits |>
      distinct(code_matched_on_name = TraitID, Trait),
    by = c(extracted_trait = "Trait")
  ) |>
  select(trait, extracted_trait, extracted_code, name_matched_on_code, code_matched_on_name)

## Potentially problematic traits
# non-matching names according to code
apd_try_smaller |>
  filter(extracted_trait != name_matched_on_code)

# non-matching code according to name
apd_try_smaller |>
  filter(extracted_code != code_matched_on_name)

# Notes:
#
# 1. Same remarks as for GIFT, some APD traits have several matching traits
#    on the same line for TRY, e.g., 'trait_0030810' has two traits matching on 'GIFT_close'.
# 2. Names are globally matching but some names correspondence are off because TRY **silently** modified the names of the trait.
#    The names can be updated accordingly by matching the TraitID in an updated TRY traits table.
# 3. More serious are the non-corresponding codes. It seems some matching are wrong because of this.
#    For example, 'leaf_cell_wall_N_per_cell_wall_dry_mass ' [APD:trait_0001511] is referenced as having a close match with 'Leaf cell wall nitrogen (N) per unit cell wall dry mass' referenced as [TRY:96] in APD, however, this traits corresponds to 'Seed oil content per seed mass'. While given the matched name it should be matching with [TRY:3377]. See the script for more example of this
