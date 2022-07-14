# Script to create network between all trait databases

# Packages ---------------------------------------------------------------------

library("dplyr")
library("tidygraph")


# Load correspondence table ----------------------------------------------------

corres_files = list.files(
  "../misc/traitharmo/data", pattern = "*_correspondence.ods",
  full.names = TRUE
)

names(corres_files) = gsub(
  "_correspondence.ods", "", basename(corres_files), fixed = TRUE
)


corres_df = lapply(corres_files, function(x) {
  readODS::read_ods(x) %>%
    as_tibble()
})


# Load Trait names -------------------------------------------------------------

targets::tar_load(austraits)
targets::tar_load(gift_traits_meta)
targets::tar_load(try_traits)

aus_names = names(austraits$definitions$traits$elements)

bien_names = BIEN::BIEN_trait_list()[["trait_name"]] %>%
  na.omit() %>%
  as.character()

gift_names = gift_traits_meta %>%
  distinct(Trait2, Lvl3)

# Checks  ----------------------------------------------------------------------

# Check that 'identical' and 'similar' have proper levels
corres_df %>%
  purrr::imap_dfr(~.x %>%
                    distinct(identical, similar) %>%
                    mutate(table = .y) %>%
                    select(table, everything()) %>%
                    arrange(table, identical, similar))

## Check names compare to initial databases
# Check AusTraits names
corres_df$austraits_bien %>%
  filter(!(austraits_trait_name %in% aus_names))

corres_df$austraits_try %>%
  filter(!(austraits_trait_name %in% aus_names))

corres_df$gift_austraits %>%
  filter(!(austraits_trait_name %in% aus_names) & !is.na(austraits_trait_name))

# Check BIEN names
corres_df$austraits_bien %>%
  filter(!(bien_trait_name) %in% bien_names)

corres_df$bien_gift %>%
  filter(!(bien_trait_name) %in% bien_names)

corres_df$bien_try %>%
  filter(!(bien_trait_name) %in% bien_names)

# Check GIFT names
corres_df$bien_gift %>%
  filter(
    (!(gift_trait_id %in% gift_names$Lvl3) |
       !(gift_trait_name %in% gift_names$Trait2)) & (!is.na(gift_trait_id) &
                                                       !is.na(gift_trait_name))
  )

corres_df$gift_austraits %>%
  filter(!(gift_trait_id %in% gift_names$Lvl3) |
           !(gift_trait_name %in% gift_names$Trait2))

corres_df$gift_try %>%
  filter(!(gift_trait_name %in% gift_names$Trait2))

# Check TRY names
corres_df$austraits_try %>%
  filter(
    (!(try_trait_id %in% try_traits$TraitID) |
       !(try_trait_name %in% try_traits$Trait)) &
      (!is.na(try_trait_id) & !is.na(try_trait_name))
  )

corres_df$bien_try %>%
  filter(
    (!(try_trait_id %in% try_traits$TraitID) |
       !(try_trait_name %in% try_traits$Trait)) &
      (!is.na(try_trait_id) & !is.na(try_trait_name))
  )

corres_df$gift_try %>%
  filter(
    (!(try_trait_id %in% try_traits$TraitID) |
       !(try_trait_name %in% try_traits$Trait)) &
      (!is.na(try_trait_id) & !is.na(try_trait_name))
  )


# Making Network of Traits -----------------------------------------------------

# Unified node data.frame
node_df = data.frame(
  name = c(aus_names, bien_names, gift_names$Trait2, try_traits$TraitID),
  database = c(
    rep("AusTraits", length.out = length(aus_names)),
    rep("BIEN",      length.out = length(bien_names)),
    rep("GIFT",      length.out = nrow(gift_names)),
    rep("TRY",       length.out = nrow(try_traits))
  )
)

# Unified edge data.frame
corres_df$gift_austraits = corres_df$gift_austraits %>%
  select(-gift_trait_id)

corres_df$bien_gift = corres_df$bien_gift %>%
  select(-gift_trait_id)

edge_df = corres_df %>%
  purrr::map_dfr(
    function(x) {
      smaller_df = x %>%
        select(1:2, identical, similar) %>%
        mutate(across(.fns = as.character))

      colnames(smaller_df)[1:2] = c("from", "to")

      smaller_df %>%
        filter(!is.na(to))
    })

edge_df %>%   # Should given nothing
  filter(is.na(identical))

# Create network
trait_network = tbl_graph(
  nodes = node_df %>%
    mutate(name = gsub(" ", "__", name, fixed = TRUE)),
  edges = edge_df %>%
    mutate(from = gsub(" ", "__", from, fixed = TRUE),
           to   = gsub(" ", "__",   to, fixed = TRUE))
)

# Plot it
plot_trait_network = ggraph(trait_network, layout = 'graphopt') +
  geom_edge_link(aes(colour = identical), edge_width = 1) +
  geom_node_point(aes(shape = database)) +
  labs(
    shape = "Trait Database", edge_colour = "Are Traits Identical or Similar?"
  ) +
  scale_edge_color_discrete(labels = c(yes = "identical", no = "similar")) +
  theme_void() +
  theme(legend.position = "top")


# Extract connected component --------------------------------------------------

# Extract all connected components seperately
all_components = trait_network %>%
  to_components()

component_size = all_components %>%
  purrr::map_dbl(
    ~.x %>%
      activate(nodes) %>%
      length()
  )

# Look at some of the biggest connected component
all_components[which(component_size > 10)]


db_df = data.frame(
  database = c("AusTraits", "BIEN", "GIFT", "TRY"),
  trait_name = c(
    "austraits_trait_name", "bien_trait_name", "gift_trait_name", "try_trait_id"
  )
)

# Get unified trait table
all_traits = purrr::map_dfr(all_components, function(x) {
  node_df = x %>%
    activate(nodes) %>%
    as.data.frame()

  has_only_try = length(node_df[["database"]]) == 1 &
    ("TRY" %in% node_df[["database"]])

  node_df = node_df %>%
    full_join(db_df, by = "database") %>%
    arrange(trait_name)

  # Add Consolidated Name
  if (!has_only_try) {

    first_non_na_trait = node_df %>%
      filter(!is.na(name)) %>%
      pull(name) %>%
      .[1]

    node_df = node_df %>%
      add_row(
        name = first_non_na_trait,
        trait_name = "consolidated_name"
      )

  } else {


    first_try_trait = node_df %>%
      filter(database == "TRY") %>%
      slice(1)

    try_name = try_traits %>%
      filter(TraitID == first_try_trait[["name"]]) %>%
      pull(Trait) %>%
      .[1]

    node_df = node_df %>%
      add_row(
        name = try_name,
        trait_name = "consolidated_name"
      )
  }

  node_df %>%
    select(-database) %>%
    tidyr::pivot_wider(
      names_from = trait_name, values_from = name, values_fn = list
    )
}) %>%
  tidyr::unnest(austraits_trait_name) %>%
  tidyr::unnest(bien_trait_name) %>%
  tidyr::unnest(gift_trait_name) %>%
  tidyr::unnest(try_trait_id) %>%
  tidyr::unnest(consolidated_name)

