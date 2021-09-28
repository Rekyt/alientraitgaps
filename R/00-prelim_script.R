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
  here::here("inst", "exdata", "try", "TryAccSpecies.txt")
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

glonaf_species = DBI::dbGetQuery(
  glonaf_con,
  paste0(
    "SELECT DISTINCT taxon_orig.id AS taxon_orig_id, taxon_orig.taxon_orig, ",
    "family_tpl.name AS family, taxon_orig.species_id, genus.name AS genus, ",
    "species.hybrid, species.epithet, infra_rank.name AS infra_rank, ",
    "species.epithet_infra, author.name AS author, ",
    "name_status.name AS TPL_name_status_standardized, species.tpl_id, ",
    "species.species_alt_id, taxon_orig.taxon_corrected, ",
    "taxon_orig.gen_hybrid AS taxon_orig_genus_hybrid, ",
    "taxon_orig.hybrid AS taxon_orig_hybrid, taxon_orig.abbrev, ",
    "taxon_orig.cultivar, taxon_orig.cult_name, ",
    "taxon_orig.TPL_Taxonomic_status AS TPL_name_status_orig FROM flora_orig",
    "\nLEFT JOIN list ON flora_orig.list_id = list.id\n",
    "LEFT JOIN taxon_orig ON flora_orig.taxon_orig_id = taxon_orig.id\n",
    "LEFT JOIN species ON taxon_orig.species_id = species.id\n",
    "LEFT JOIN genus ON species.genus_id = genus.id\n",
    "LEFT JOIN family_tpl ON genus.family_tpl_id = family_tpl.id\n",
    "LEFT JOIN infra_rank ON species.infra_rank_id = infra_rank.id\n",
    "LEFT JOIN author ON species.author_id = author.id\n",
    "LEFT JOIN name_status ON species.name_status_id = name_status.id\n",
    "WHERE list.outdated = 0"
  )
)

glonaf_list = glonaf_species %>%
  distinct(taxon_corrected) %>%
  mutate(taxon_corrected = stringr::str_trim(taxon_corrected)) %>%
  pull(taxon_corrected) %>%
  unique()


# Harmonize both taxonomies ----------------------------------------------------

## Ok weird error, will skip for now
# match_lvcp = lcvplants::lcvp_match(
#   try_species %>%
#     filter(validEnc(AccSpeciesName),
#            grepl(" ", AccSpeciesName, fixed = TRUE)) %>%
#     pull(AccSpeciesName),
#   glonaf_list[["taxon_corrected"]]
# )


## Doesn't work either for weird reasons (encoding?)
# match_lcvp_glonaf = lcvplants::lcvp_search(glonaf_list)

## Set up World Flora Online
# WorldFlora::WFO.download(save.dir = here::here("inst", "exdata", "wfo"))

# match_wfo_glonaf = WorldFlora::WFO.match(
#   glonaf_species %>%
#     distinct(taxon_corrected) %>%
#     mutate(taxon_corrected = stringr::str_trim(taxon_corrected)) %>%
#     rename(spec.name = taxon_corrected),
#   WFO.file = here::here("inst", "exdata", "wfo", "classification.txt"))


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
