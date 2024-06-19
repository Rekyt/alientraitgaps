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
      distinct(consolidated_name, austraits_trait_name),
    by = c(trait_name = "austraits_trait_name")
  )
bien_simple = bien_traits |>
  distinct(scrubbed_species_binomial, trait_name) |>
  inner_join(
    potential_traits |>
      distinct(consolidated_name, bien_trait_name),
    by = c(trait_name = "bien_trait_name")
  )

gift_simple = gift_traits |>
  distinct(trait_ID, work_species) |>
    inner_join(
      tar_read(gift_trait_meta) |>
        distinct(trait_ID = Lvl3, gift_trait_name = Trait2)
    ) |>
  inner_join(
    potential_traits |>
      distinct(consolidated_name, gift_trait_name),
    by = "gift_trait_name"
  )

try_simple = try_traits |>
  distinct(AccSpeciesName, TraitID) |>
  inner_join(
    potential_traits |>
      distinct(consolidated_name, TraitID = as.integer(try_trait_id)),
    by = "TraitID"
  )

total_traits = list(
  austraits = austraits_simple |>
    distinct(species = taxon_name, consolidated_name),
  bien = bien_simple |>
    distinct(species = scrubbed_species_binomial, consolidated_name),
  gift = gift_simple |>
    distinct(species = work_species, consolidated_name),
  try = try_simple |>
    distinct(species = AccSpeciesName, consolidated_name)
) |>
  bind_rows(.id = "database")

total_diaz = total_traits |>
  group_by(species) |>
  summarise(traits = list(unique(consolidated_name))) |>
  rowwise() |>
  mutate(
    has_diaz = ifelse(
      any(apply(exact_combs$diaz, 1, \(x) all(x %in% traits))), TRUE, FALSE
    )
  ) |>
    ungroup()

with_diaz = total_diaz |>
  filter(has_diaz)

# Filter species with full GSPFF -----------------------------------------------

austraits_gspff = austraits_traits |>
  filter(taxon_name %in% with_diaz$species)

bien_gspff = bien_traits |>
  filter(scrubbed_species_binomial %in% with_diaz$species)

gift_gspff = gift_traits |>
  filter(work_species %in% with_diaz$species)

try_gspff = try_traits |>
  filter(AccSpeciesName %in% with_diaz$species)

# Save all these data ----------------------------------------------------------

folder = here::here("inst", "data_extracts", "sgubik_extract")

saveRDS(
  austraits_gspff,
  here::here(folder, "austraits_gspff.Rds")
)
saveRDS(
  bien_gspff,
  here::here(folder, "bien_gspff.Rds")
)
saveRDS(
  gift_gspff,
  here::here(folder, "gift_gspff.Rds")
)
saveRDS(
  try_gspff,
  here::here(folder, "try_gspff.Rds")
)

tictoc::toc()
