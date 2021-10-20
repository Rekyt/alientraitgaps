# Preliminary script to make figures for GloNAF talk

# Packages ---------------------------------------------------------------------
library("dplyr")
library("ggplot2")

# Functions --------------------------------------------------------------------

# Load TRY data ----------------------------------------------------------------
try_df = disk.frame::disk.frame(
  here::here("inst", "exdata", "try", "12477.df")
)

try_species = data.table::fread(
  here::here("inst", "exdata", "try", "TryAccSpecies.txt"),
  encoding = "UTF-8"
)

try_list = try_species %>%
  filter(validEnc(AccSpeciesName),
         grepl(" ", AccSpeciesName, fixed = TRUE)) %>%
  pull(AccSpeciesName)

# Load GloNAF data -------------------------------------------------------------
glonaf_con = DBI::dbConnect(
  RMariaDB::MariaDB(),
  dbname = "glonafdb",
  username = Sys.getenv("GLONAF_USER"),
  password = Sys.getenv("GLONAF_PASSWORD"),
  host = "134.34.205.46",
  port = 6609
)

# Count number of species per alien status
glonaf_count_alien_status = tbl(glonaf_con, "flora_orig") %>%
  select(taxon_orig_id, status_id) %>%
  distinct() %>%
  count(status_id, sort = TRUE) %>%
  inner_join(tbl(glonaf_con, "status"), by = c("status_id" = "id")) %>%
  collect()

# Filter really alien species
glonaf_alien_species = tbl(glonaf_con, "flora_orig") %>%
  # Get taxa that are referenced as naturalized, aliens, or invasive
  filter(status_id %in% c(2, 4, 5, 7)) %>%
  distinct(taxon_orig_id) %>%
  # Get species names and ids
  inner_join(tbl(glonaf_con, "taxon_orig"), by = c("taxon_orig_id" = "id")) %>%
  # Corrected names after matching TPL
  distinct(species_id) %>%
  inner_join(tbl(glonaf_con, "species"), by = c("species_id" = "id")) %>%
  select(-species_id) %>%
  # Add Name status from TPL
  inner_join(tbl(glonaf_con, "name_status"), by = c(name_status_id = "id")) %>%
  select(-name_status_id) %>%
  # Get only binomial name
  filter(infra_rank_id == 4) %>%
  # Add full genus name
  inner_join(glonaf_con %>%
               tbl("genus") %>%
               select(genus_id = id, genus = name), by = "genus_id") %>%
  select(-genus_id) %>%
  # Add author name
  inner_join(glonaf_con  %>%
               tbl("author") %>%
               select(author_id = id, author_name = name),
             by = "author_id") %>%
  select(-author_id) %>%
  collect()

glonaf_list = glonaf_alien_species %>%
  filter(name == "accepted") %>%
  distinct(genus, epithet, author_name) %>%
  mutate(full_name = paste(genus, epithet, author_name))


# Match TRY using TNRS ---------------------------------------------------------
tictoc::tic(msg = "Time elapsed for matching TRY species: ")
match_try_tnrs = TNRS::TNRS(try_list)
tictoc::toc()

saveRDS(match_try_tnrs, "inst/cleaned_data/match_try_tnrs.Rds")


# Match GloNAF using TNRS ------------------------------------------------------
tictoc::tic(msg = "Time elapsed for matching GloNAF species: ")
match_glonaf_tnrs = TNRS::TNRS(glonaf_list[["full_name"]])
tictoc::toc()

saveRDS(match_glonaf_tnrs, "inst/cleaned_data/match_glonaf_tnrs.Rds")


# Harmonize both taxonomies ----------------------------------------------------

# Load Matched TNRS names
match_try_tnrs = readRDS("inst/cleaned_data/match_try_tnrs.Rds")

match_glonaf_tnrs = readRDS("inst/cleaned_data/match_glonaf_tnrs.Rds")

# Subset most important columns
sub_try = match_try_tnrs %>%
  distinct(
    name_init_try        = Name_submitted,
    status_try           = Taxonomic_status,
    name_accepted_try    = Accepted_name,
    species_accepted_try = Accepted_species,
    author_accepted_try  = Author_matched
  )

sub_glonaf = match_glonaf_tnrs %>%
  distinct(
    name_init_glonaf        = Name_submitted,
    status_glonaf           = Taxonomic_status,
    name_accepted_glonaf    = Accepted_name,
    species_accepted_glonaf = Accepted_species,
    author_accepted_glonaf  = Author_matched
  )

# Merge TRY and GloNAF accepted names
harmonized_try_glonaf = sub_try %>%
  filter(species_accepted_try != "") %>%
  inner_join(
    sub_glonaf %>%
      filter(species_accepted_glonaf != ""),
    by = c(species_accepted_try = "species_accepted_glonaf")
  )


# Compute number of trait available --------------------------------------------

# Count number of traits per consolidated species
try_number_trait = harmonized_try_glonaf %>%
  # Getting back TRY species IDs
  inner_join(try_species,
             by = c(name_init_try = "AccSpeciesName")) %>%
  # Regrouping species with similar harmonized names
  group_by(species_accepted_try) %>%
  summarise(
    observation_number = sum(ObsNum),
    trait_number       = sum(TraitNum),
    measure_number     = sum(MeasNum),
    georef_obs_number  = sum(ObsGRNum),
    georef_measure_number = sum(MeasGRNum))


fig_num_trait = try_number_trait %>%
  ggplot(aes(trait_number)) +
  geom_histogram(color = "white") +
  scale_x_log10(name = "Number of ≠ traits in TRY") +
  scale_y_continuous(name = "Number of species") +
  labs(title = "GloNAF species in TRY (~15k)",
       caption = "GloNAF species harmonized using TNRS") +
  theme_bw() +
  theme(aspect.ratio = 1,
        panel.grid = element_blank())

ggsave(
  plot = fig_num_trait,
  filename =  here::here("inst", "figures",
                         "figure_glonaf_try_number_traits.png")
)


# Which traits are available in TRY for GloNAF species -------------------------

glonaf_try_traits_available = harmonized_try_glonaf %>%
  # Getting back TRY species IDs
  inner_join(try_species %>%
               select(AccSpeciesID, AccSpeciesName),
             by = c(name_init_try = "AccSpeciesName")) %>%
  # Add TRY traits
  inner_join(try_df %>%
               filter(!is.na(TraitID)) %>%
               collect(), by = "AccSpeciesID")

# Count number of species per trait measured
try_number_species_per_trait = glonaf_try_traits_available %>%
  distinct(species_accepted_try, TraitID, TraitName) %>%
  count(TraitName, sort = TRUE, name = "n_sp") %>%
  mutate(TraitName = factor(TraitName) %>%
           forcats::fct_reorder(n_sp))

# Show number of species per trait
fig_detail_traits = try_number_species_per_trait %>%
  slice_max(n_sp, n = 15) %>%
  ggplot(aes(n_sp, TraitName)) +
  geom_point() +
  scale_x_log10(name = "Number of species measured") +
  scale_y_discrete(labels = scales::wrap_format(25)) +
  labs(y = "Trait name",
       title = "15 Most frequent trait in TRY from GloNAF species (15k)",
       caption = "TRY open data on a selected subset of traits") +
  theme_bw() +
  theme(aspect.ratio = 1)

fig_detail_traits


## Trait combination
# What is the most commonly measured trait combination?
try_trait_combination = glonaf_try_traits_available %>%
  distinct(species_accepted_try, TraitID, TraitName) %>%
  tibble::as_tibble() %>%
  select(-TraitID) %>%
  group_by(species_accepted_try) %>%
  summarise(trait_names = list(TraitName))

fig_glonaf_combine_traits = try_trait_combination %>%
  ggplot(aes(x = trait_names)) +
  geom_bar() +
  geom_text(stat='count', aes(label = after_stat(count)), vjust = -1) +
  ggupset::scale_x_upset(n_intersections = 8) +
  labs(caption = "TRY open data on a selected subset of traits",
       subtitle = "Most commonly measured combination of traits on aliens")

fig_glonaf_combine_traits

# Location of trait measurements -----------------------------------------------


# Querying traits from BIEN ----------------------------------------------------

query_bien_traits = harmonized_try_glonaf %>%
  pull(species_accepted_try) %>%
  unique() %>%
  BIEN::BIEN_trait_species()

# Get InvaCost data ------------------------------------------------------------


