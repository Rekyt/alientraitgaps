tar_load(consolidated_trait_names)

head(consolidated_trait_names)

growth_form_traits = consolidated_trait_names %>%
  filter(grepl("growth form", bien_trait_name))

bien_growth_form = tar_read(glonaf_bien_traits) %>%
  filter(grepl("whole plant growth form",trait_name)) %>%
  distinct(species = scrubbed_species_binomial, trait_name, trait_value)

try_growth_form = tar_read(glonaf_try_traits_available) %>%
  filter(TraitID %in% unique(growth_form_traits$try_trait_id))

gift_growth_form = tar_read(gift_traits_final) %>%
  filter(trait_ID == "1.2.1") %>%
  distinct(work_ID, trait_value) %>%
  inner_join(
    tar_read(gift_names_traits) %>%
      distinct(work_ID, species),
    by = "work_ID"
  ) %>%
  inner_join(
    tar_read(harmonized_gift_glonaf) %>%
      distinct(species = name_init_gift, species_accepted_gift),
    by = "species"
  ) %>%
  distinct(species = species_accepted_gift, trait_value) %>%
  select(species, trait_value)
