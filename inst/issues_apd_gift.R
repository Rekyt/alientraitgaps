# Investigating correspondence between trait code and trait name in external DB
# in AusTraits
library("dplyr")
library("targets")

tar_load(apd_subset)
tar_load(gift_trait_meta)

gift_trait_meta = GIFT::GIFT_traits_meta()

tibble::as_tibble(read.csv("APD_traits_input.csv")) |>
  select(identifier:label, starts_with("GIFT")) |>


apd_gift_detailed = apd_subset |>
  select(trait_id:label, starts_with("GIFT")) |>
  tidyr::pivot_longer(
    starts_with("GIFT"), names_to = "match_type", values_to = "matched_trait"
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
      split_traits, \(x) stringr::str_extract(x, "\\[GIFT:(.+)\\]", group = 1)
    ),
    # Get level
    gift_lvl = purrr::map(
      extracted_code, \(x) stringr::str_count(x, stringr::fixed(".")) + 1L
    )
  ) |>
  # Put everything in a tidy format
  tidyr::unnest(split_traits:gift_lvl)

## Level 2 traits
# Matching code at level 2
apd_gift_lvl2 = apd_gift_detailed |>
  filter(gift_lvl == 2) |>
    left_join(
      gift_trait_meta |>
        distinct(Lvl2, Trait1),
      by = c(extracted_code = "Lvl2")
    )

# Problematic traits
apd_gift_lvl2 |>
  filter((extracted_trait != Trait1) | is.na(Trait1))


## Level 3 traits
apd_gift_lvl3 = apd_gift_detailed |>
  filter(gift_lvl == 3) |>
  left_join(
    gift_trait_meta |>
      distinct(Lvl3, Trait2),
    by = c(extracted_code = "Lvl3")
  )

# Problematic traits
apd_gift_lvl3 |>
  filter((extracted_trait != Trait2) | is.na(Trait2))

# Notes:
#
# 1. for trait_0030020 & trait_0030015 GIFT_close contains multiple traits
#    is it on purpose, while for other traits it's on multiple lines?.
# 2. for trait_0030215, typo GIFT name is "Fuiting time" missing an 'r'.
# 3. for trait_0030020, typo in GIFT code 'leaf_thorns_1 [GIFT:4:14.1]'
#    should be 'leaf_thorns_1 [GIFT**.**4:14.1].
# 4. Several traits names are written following AusTraits' convention and not
#    GIFT's 'seed_height' instead of Seed height.
# 5. Capitalization of trait names isn't following GIFT's names, AusTraits tend
#    to use snake_case while GIFT uses Camel_snake_case
# 6. Error in trait_0030060, GIFT_close matches with GIFT 1.4.1 (Climber_1)
#    while it should match with GIFT 3.4.1 (Reproduction_sexual_1)
