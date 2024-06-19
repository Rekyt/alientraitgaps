# Script to perform extracts for sGubik
#
# # Packages ---------------------------------------------------------------------
library("dplyr")
library("targets")

# Load trait data --------------------------------------------------------------
# Diaz combinations

tar_load(trait_names_exact)

exact_combs = get_exact_combs()

diaz_traits = as.vector(exact_combs$diaz) |>
  stack() |>
  pull(values) |>
  unique()

potential_traits = trait_names_exact |>
  filter(consolidated_name %in% diaz_traits)

# Extract trait data -----------------------------------------------------------

# AusTraits
austraits_traits = tar_read(austraits)$traits |>
  filter(trait_name %in% na.omit(unique(potential_traits$austraits_trait_name)))

# BIEN
bien_traits = tar_read(bien_traits) |>
  filter(trait_name %in% na.omit(unique(potential_traits$bien_trait_name)))

# GIFT
gift_trait_ids = tar_read(gift_trait_meta) |>
  distinct(Lvl3, Trait2) |>
  filter(Trait2 %in% na.omit(unique(potential_traits$gift_trait_name))) |>
  pull(Lvl3)

gift_traits = tar_read(gift_raw_traits) |>
  filter(trait_ID %in% gift_trait_ids)

# TRY
try_traits = tar_read(full_try_df) |>
  filter(TraitID %in% na.omit(unique(potential_traits$try_trait_id)))


# Compute number of GSPFF species ----------------------------------------------

austraits_simple = austraits_traits |>
  distinct(taxon_name, trait_name) |>
  inner_join(
    potential_traits |>
      distinct(consolidated_name, austraits_trait_name)
    by = c(trait_name = "austraits_trait_name")
  )
bien_simple = bien_traits |>
  distinct(scrubbed_species_binomial, trait_name) |>

# Save all these data ----------------------------------------------------------
if (dir.exists(here::here("inst", "data_extracts", "weihan_data"))) {

  dir.create(here::here("inst", "data_extracts", "weihan_data"),
             recursive = TRUE)
}

saveRDS(
  austraits_traits,
  here::here("inst", "data_extracts", "weihan_data", "austraits_weihan.Rds")
)
saveRDS(
  bien_traits,
  here::here("inst", "data_extracts", "weihan_data", "bien_weihan.Rds")
)
saveRDS(
  gift_traits,
  here::here("inst", "data_extracts", "weihan_data", "gift_weihan.Rds")
)
saveRDS(
  try_traits,
  here::here("inst", "data_extracts", "weihan_data", "try_weihan.Rds")
)

tictoc::toc()
