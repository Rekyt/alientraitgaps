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

match_try_tnrs = readRDS("inst/cleaned_data/match_try_tnrs.Rds")

match_glonaf_tnrs = readRDS("inst/cleaned_data/match_glonaf_tnrs.Rds")

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

harmonized_try_glonaf = sub_try %>%
  filter(species_accepted_try != "") %>%
  inner_join(
    sub_glonaf %>%
      filter(species_accepted_glonaf != ""),
    by = c(species_accepted_try = "species_accepted_glonaf")
  )

# Exact matching between TRY and GloNAF ----------------------------------------

# Proceeds with exact matching
exact_try_glonaf = try_species %>%
  right_join(glonaf_species %>%
              distinct(species_id, genus, epithet) %>%
              transmute(species = paste(genus, epithet, sep = " ")),
            by = c(AccSpeciesName = "species")) %>%
  mutate(TraitNum = ifelse(is.na(TraitNum), 0, TraitNum))


# Problematic harmonization with LCVP and GloNAF -------------------------------

binomial_glonaf = glonaf_species %>%
  distinct(taxon_corrected) %>%
  mutate(n_spaces = stringr::str_count(taxon_corrected, " "))

problematic_species = binomial_glonaf %>%
  filter(n_spaces == 0) %>%
  head() %>%
  mutate(taxon_corrected = substr(taxon_corrected, 1, nchar(taxon_corrected) - 1))

lcvplants::lcvp_search(problematic_species$taxon_corrected)

match_glonaf_lcvp = lcvplants::lcvp_search(glonaf_species[["taxon_corrected"]])



# Compute number of trait available --------------------------------------------

fig_num_trait = exact_try_glonaf %>%
  ggplot(aes(TraitNum)) +
  geom_histogram(color = "white") +
  scale_x_log10(name = "Number of diff. Traits in TRY") +
  scale_y_sqrt(name = "Number of species") +
  labs(subtitle = "GloNAF aliens in TRY (19k / 21k)") +
  theme_bw() +
  theme(aspect.ratio = 1,
        panel.grid = element_blank())

ggsave(
  plot = fig_num_trait,
  filename =  here::here("inst", "figures",
                         "figure_glonaf_try_number_traits.png")
)


# What traits are available?
fig_detail_traits = exact_try_glonaf %>%
  filter(TraitNum != 0) %>%
  distinct(AccSpeciesID) %>%
  inner_join(try_df %>%
               filter(!is.na(TraitID)) %>%
               collect(), by = "AccSpeciesID") %>%
  distinct(AccSpeciesID, AccSpeciesName, TraitID, TraitName) %>%
  count(TraitName, sort = TRUE, name = "n_sp") %>%
  mutate(TraitName = factor(TraitName) %>%
           forcats::fct_reorder(n_sp)) %>%
  slice_max(n_sp, n = 15) %>%
  ggplot(aes(n_sp, TraitName)) +
  geom_point() +
  scale_x_log10(name = "Number of species measured") +
  scale_y_discrete(labels = scales::wrap_format(20)) +
  labs(y = "Trait name", subtitle = "Most measured trait on 19k GloNAF species",
       caption = "TRY open data on a selected subset of traits") +
  theme_bw() +
  theme(aspect.ratio = 1)

fig_detail_traits


# What is the most commonly measured trait combination?
try_traits = exact_try_glonaf %>%
  filter(TraitNum != 0) %>%
  distinct(AccSpeciesID) %>%
  inner_join(try_df %>%
               filter(!is.na(TraitID)) %>%
               collect(), by = "AccSpeciesID") %>%
  distinct(AccSpeciesID, AccSpeciesName, TraitID, TraitName)

fig_glonaf_combine_traits = try_traits %>%
  tibble::as_tibble() %>%
  select(-AccSpeciesID,-TraitID) %>%
  group_by(AccSpeciesName) %>%
  summarise(trait_names = list(TraitName)) %>%
  ggplot(aes(x = trait_names)) +
  geom_bar() +
  geom_text(stat='count', aes(label = after_stat(count)), vjust = -1) +
  ggupset::scale_x_upset(n_intersections = 8) +
  labs(caption = "TRY open data on a selected subset of traits",
       subtitle = "Most commonly measured combination of traits on aliens")

fig_glonaf_combine_traits

# Location of trait measurements -----------------------------------------------
