# Script to extract traits for Weihan
# Packages ---------------------------------------------------------------------
library("dplyr")
library("targets")

# Load trait data --------------------------------------------------------------
# 25 most measured traits + Diaz combinations

tar_load(combined_traits_full)
tar_load(simplified_traits_full)
tar_load(trait_names_full)


# Count 25
top_25_traits = simplified_traits_full |>
  count(consolidated_name, sort = TRUE) |>
  slice_head(n = 25)

traits_to_keep = trait_names_full |>
  filter(
    consolidated_name %in%
      c(top_25_traits$consolidated_name, "Wood density",
        "Leaf dry matter content (LDMC)",
        "Leaf nitrogen (N) content per unit leaf dry mass",
        "Leaf area")
  )

# Extract trait data -----------------------------------------------------------

# AusTraits
austraits_traits = austraits$traits |>
  filter(trait_name %in% na.omit(unique(traits_to_keep$austraits_trait_name)))

# BIEN
bien_traits = tar_read(bien_traits) |>
  filter(trait_name %in% na.omit(unique(traits_to_keep$bien_trait_name)))

# GIFT
gift_trait_ids = tar_read(gift_trait_meta) |>
  distinct(Lvl3, Trait2) |>
  filter(Trait2 %in% na.omit(unique(traits_to_keep$gift_trait_name))) |>
  pull(Lvl3)

gift_traits = tar_read(gift_raw_traits) |>
  filter(trait_ID %in% gift_trait_ids)

# TRY
try_traits = tar_read(full_try_df) |>
  filter(TraitID %in% na.omit(unique(traits_to_keep$try_trait_id)))


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
