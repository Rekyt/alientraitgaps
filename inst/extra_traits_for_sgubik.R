# Extracting all species that have diaz information

# Packages ---------------------------------------------------------------------

library("targets")
library("dplyr")
library("disk.frame")


# Load needed targets ----------------------------------------------------------

tar_load(network_consolidated_trait_names)
tar_load(austraits)
tar_load(full_try_df)
tar_load(match_try_tnrs)
tar_load(match_raw_gift_tnrs)
tar_load(match_austraits_tnrs)


# Select traits ----------------------------------------------------------------

# Filter subset of traits needed for GSPFF
diaz_traits = network_consolidated_trait_names %>%
  filter(
    consolidated_name %in%
      c("specific_leaf_area", "diaspore_mass", "plant_height", "leaf_area",
        "wood_density", "leaf_N_per_dry_mass")
  )

# AusTraits
austraits_traits = diaz_traits %>%
  pull(aus_trait_name) %>%
  unique()

austraits_trait_species = austraits$traits %>%
  filter(trait_name %in% austraits_traits)

saveRDS(austraits_trait_species, "inst/exdata/austraits_species.Rds")

# BIEN
bien_species = BIEN::BIEN_trait_trait(
  diaz_traits %>%
    pull(bien_trait_name) %>%
    unique()
)

saveRDS(bien_species, "inst/exdata/bien_diaz_traits.Rds")

# GIFT
gift_traits = GIFT::GIFT_traits_meta() %>%
  filter(
    Trait2 %in% {diaz_traits %>%
        pull(gift_trait_name) %>%
        unique() %>%
        na.omit() %>%
        as.vector()}
  )

gift_traits_species = GIFT::GIFT_traits_raw(gift_traits %>%
                                              pull(Lvl3))

saveRDS(gift_traits_species, "inst/exdata/gift_trait_species.Rds")

# TRY
try_traits = diaz_traits %>%
  pull(try_trait_id) %>%
  unique() %>%
  na.omit() %>%
  as.vector()

try_trait_species = full_try_df %>%
  filter(TraitID %in% try_traits) %>%
  collect()

saveRDS(try_trait_species, "inst/exdata/try_trait_species.Rds")


## Selecting species with all traits -------------------------------------------

austraits_trait_species = readRDS("inst/exdata/austraits_species.Rds")
matched_austraits_traits = austraits_trait_species %>%
  full_join(
    match_austraits_tnrs %>%
      distinct(Name_submitted, Accepted_species),
    by = c(original_name = "Name_submitted")
  )


bien_diaz_trait = readRDS("inst/exdata/bien_diaz_traits.Rds")
matched_bien_traits = bien_diaz_trait

gift_trait_species = readRDS("inst/exdata/gift_trait_species.Rds")
matched_gift_traits = gift_trait_species

try_trait_species = readRDS("inst/exdata/try_trait_species.Rds")
matched_try_traits = try_trait_species %>%
  full_join(
    match_try_tnrs %>%
      distinct(Name_submitted, Accepted_species),
    by = c(AccSpeciesName = "Name_submitted")
  )

austraits_avail = matched_austraits_traits %>%
  distinct(Accepted_species, trait_name) %>%
  inner_join(
    network_consolidated_trait_names %>%
      distinct(aus_trait_name, consolidated_name) %>%
      filter(!is.na(aus_trait_name)),
    by = c(trait_name = "aus_trait_name")
  )

bien_avail = matched_bien_traits %>%
  distinct(Accepted_species = scrubbed_species_binomial, trait_name) %>%
  inner_join(
    network_consolidated_trait_names %>%
      distinct(bien_trait_name, consolidated_name) %>%
      filter(!is.na(bien_trait_name)),
    by = c(trait_name = "bien_trait_name")
  )


gift_avail = matched_gift_traits %>%
  distinct(Accepted_species = work_species, trait_ID) %>%
  inner_join(
    gift_traits %>%
      select(trait_ID = Lvl3, trait_name = Trait2),
    by = "trait_ID"
  ) %>%
  inner_join(
    network_consolidated_trait_names %>%
      distinct(gift_trait_name, consolidated_name) %>%
      filter(!is.na(gift_trait_name)),
    by = c(trait_name = "gift_trait_name")
  )


try_avail = matched_try_traits %>%
  distinct(Accepted_species, TraitName, TraitID) %>%
  inner_join(
    network_consolidated_trait_names %>%
      distinct(try_trait_id, consolidated_name) %>%
      mutate(try_trait_id = as.integer(try_trait_id)) %>%
      filter(!is.na(try_trait_id)),
    by = c(TraitID = "try_trait_id")
  )

total_avail = list(
  austraits = austraits_avail,
  bien      = bien_avail,
  gift      = gift_avail,
  try       = try_avail) %>%
  purrr::imap(function(x, y) {
    x %>%
      distinct(Accepted_species, consolidated_name) %>%
        group_by(Accepted_species) %>%
        summarise({{y}} := lst(consolidated_name))
  }) %>%
    {Reduce(function(x, y) full_join(x, y, by = "Accepted_species"), .)}


# Get all traits available across databases
full_traits = total_avail %>%
  filter(Accepted_species != "") %>%
  rowwise() %>%
  mutate(
    total_traits = list(
      Reduce(function(x, y) union(x, y), list(austraits, bien, gift, try))
    )
  ) %>%
  ungroup()

saveRDS(full_traits, "inst/exdata/total_available.Rds")


full_traits %>%
  filter(
    purrr::map_lgl(total_traits, \(x) length(x) == 6)
  ) %>%
  saveRDS("inst/exdata/diaz_availables.Rds")
