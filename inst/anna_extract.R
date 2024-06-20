# Script to extract traits for all of Anna's species

# Packages ---------------------------------------------------------------------
library("dplyr")
library("targets")

# Load initial data ------------------------------------------------------------
folder = here::here("inst", "data_extracts", "anna_extract")

host_species = readRDS(here::here(folder, "host_species_names.rds"))

annas_species = unique(host_species$Taxon)

# Get trait data ---------------------------------------------------------------

austraits_traits = tar_read(austraits)$traits |>
  filter(taxon_name %in% annas_species)

bien_traits = tar_read(bien_traits) |>
  filter(scrubbed_species_binomial %in% annas_species)

gift_traits = tar_read(gift_raw_traits) |>
  inner_join(
    tar_read(gift_trait_meta) |>
      distinct(trait_ID = Lvl3, trait_name = Trait2),
    by = "trait_ID"
  ) |>
    filter(work_species %in% annas_species)

try_traits = tar_read(full_try_df) |>
  semi_join(
    tar_read(try_harmonized_species) |>
      filter(MatchedName %in% annas_species) |>
        rename(AccSpeciesID = TRY_SpeciesID),
    by = "AccSpeciesID"
  )

# Save data --------------------------------------------------------------------

saveRDS(
  austraits_traits,
  here::here(folder, "austraits_anna.Rds")
)
saveRDS(
  bien_traits,
  here::here(folder, "bien_anna.Rds")
)
saveRDS(
  gift_traits,
  here::here(folder, "gift_anna.Rds")
)
saveRDS(
  try_traits,
  here::here(folder, "try_anna.Rds")
)

saveRDS(
  tar_read(gift_trait_meta) |>
    rename(trait_ID = Lvl3),
  here::here(folder, "gift_trait_meta.Rds")
)
