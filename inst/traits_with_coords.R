tar_load(
  c("austraits_coords", "try_traits_coords", "bien_traits_coords",
    "consolidated_trait_names", "match_austraits_tnrs", "match_try_tnrs")
)


# Add trait names --------------------------------------------------------------

dim(austraits_coords)
austraits_coords = austraits_coords %>%
  # Add consolidated name
  inner_join(
    consolidated_trait_names %>%
      distinct(aus_trait_name, consolidated_name) %>%
      filter(!is.na(aus_trait_name)),
    by = c(trait_name = "aus_trait_name")
  )
dim(austraits_coords)


dim(bien_traits_coords)
bien_traits_coords = bien_traits_coords %>%
  # Add consolidated name
  inner_join(
    consolidated_trait_names %>%
      distinct(bien_trait_name, consolidated_name) %>%
      filter(!is.na(bien_trait_name)),
    by = c(trait_name = "bien_trait_name")
  )
dim(bien_traits_coords)


dim(try_traits_coords)
try_traits_coords = try_traits_coords %>%
  # Add consolidated name
  inner_join(
    consolidated_trait_names %>%
      distinct(try_trait_id, try_trait_name, consolidated_name) %>%
      filter(!is.na(try_trait_name)),
    by = c(TraitID = "try_trait_id", TraitName = "try_trait_name")
  )
dim(try_traits_coords)


# Add harmonized species names -------------------------------------------------

austraits_coords_species = austraits_coords %>%
  inner_join(
    tar_read(harmonized_austraits_glonaf) %>%
      distinct(name_init_austraits, species_accepted_austraits),
    by = c(taxon_name = "name_init_austraits")
  ) %>%
  select(
    consolidated_name, species_name = species_accepted_austraits, has_coords
  )

bien_traits_coords_species = bien_traits_coords %>%
  select(
    consolidated_name, species_name = scrubbed_species_binomial, has_coords
  )

try_traits_coords_species = try_traits_coords %>%
  inner_join(
    tar_read(harmonized_try_glonaf) %>%
      distinct(name_init_try, species_accepted_try),
    by = c(AccSpeciesName = "name_init_try")
  ) %>%
  select(consolidated_name, species_name = species_accepted_try, has_coords)

all_coords = list(
  austraits_coords_species,
  bien_traits_coords_species,
  try_traits_coords_species
) %>%
  bind_rows()

ko = all_coords %>%
  semi_join(
    all_coords %>%
      count(consolidated_name) %>%
      slice_max(n, n = 16) %>%
      distinct(consolidated_name),
    by = "consolidated_name"
  )

ko %>%
  mutate(
    consolidated_name = consolidated_name %>%
      factor() %>%
      forcats::fct_infreq()
  ) %>%
  ggplot(aes(x = consolidated_name, fill = has_coords)) +
  geom_bar() +
  labs(x = "Trait Name", y = "Number of Species per Trait Obs") +
  scale_fill_brewer(
    "Has Coordinates?", labels = c(`FALSE` = "No", `TRUE` = "Yes"),
    palette = "Set1"
  ) +
  theme_bw() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, size = 8, hjust = 1)
  )
