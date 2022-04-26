# Functions to wrangle GIFT trait data specifically

# Outputs a correspondence table between GIFT trait names and TRY IDs
make_gift_try_traits_correspond = function(gift_traits_meta) {
  gift_traits_meta %>%
    mutate(
      trait_ids =
        case_when(
          # Morphological Traits
          Trait2 == "Woodiness_1"              ~ list(38),
          Trait2 == "Shoot_length_min"         ~ list(NA_real_),
          Trait2 == "Shoot_length_max"         ~ list(NA_real_),
          Trait2 == "Growth_form_1"            ~ list(c(42, 3400, 3401)),
          Trait2 == "Growth_form_2"            ~ list(c(42, 3400, 3401)),
          Trait2 == "Epiphyte_1"               ~ list(c(42, 343)),
          Trait2 == "Epiphyte_2"               ~ list(c(42, 343)),
          Trait2 == "Climber_1"                ~ list(42),
          Trait2 == "Climber_2"                ~ list(42),
          Trait2 == "Parasite_1"               ~ list(42),
          Trait2 == "Plant_height_min"         ~ list(c(3106, 3107)),
          Trait2 == "Plant_height_max"         ~ list(c(3106, 3107)),
          Trait2 == "Plant_height_mean"        ~ list(c(3106, 3107)),
          Trait2 == "Plant_height_measured"    ~ list(c(3106, 3107)),
          Trait2 == "Aquatic_1"                ~ list(42),
          Trait2 == "DBH_min"                  ~ list(21),
          Trait2 == "DBH_max"                  ~ list(21),
          Trait2 == "DBH_mean"                 ~ list(21),
          Trait2 == "DBH_measured"             ~ list(21),
          Trait2 == "Shoot_orientation"        ~ list(NA_real_),
          # Life-history traits
          Trait2 == "Lifecycle_1"              ~ list(59),
          Trait2 == "Lifespan_1"               ~ list(59),
          Trait2 == "Lifespan_2"               ~ list(59),
          Trait2 == "Life_form_1"              ~ list(343),
          Trait2 == "Life_form_2"              ~ list(343),
          Trait2 == "Deciduousness_1"          ~ list(37),
          # Reproduction traits
          Trait2 == "Self_fertilization_1"     ~ list(204),
          Trait2 == "Seed_length_min"          ~ list(27),
          Trait2 == "Seed_length_max"          ~ list(27),
          Trait2 == "Seed_length_mean"         ~ list(27),
          Trait2 == "Seed_length_measured"     ~ list(27),
          Trait2 == "Seed_width_min"           ~ list(239),
          Trait2 == "Seed_width_max"           ~ list(239),
          Trait2 == "Seed_width_mean"          ~ list(239),
          Trait2 == "Seed_width_measured"      ~ list(239),
          Trait2 == "Seed_height_min"          ~ list(238),
          Trait2 == "Seed_height_max"          ~ list(238),
          Trait2 == "Seed_height_mean"         ~ list(238),
          Trait2 == "Seed_height_measured"     ~ list(238),
          Trait2 == "Fruit_length_min"         ~ list(918),
          Trait2 == "Fruit_length_max"         ~ list(918),
          Trait2 == "Fruit_length_mean"        ~ list(918),
          Trait2 == "Fruit_length_measured"    ~ list(918),
          Trait2 == "Fruit_width_min"          ~ list(NA_real_),
          Trait2 == "Fruit_width_max"          ~ list(NA_real_),
          Trait2 == "Fruit_width_mean"         ~ list(NA_real_),
          Trait2 == "Fruit_width_measured"     ~ list(NA_real_),
          Trait2 == "Fruit_height_min"         ~ list(NA_real_),
          Trait2 == "Fruit_height_max"         ~ list(NA_real_),
          Trait2 == "Fruit_height_mean"        ~ list(NA_real_),
          Trait2 == "Fruit_height_measured"    ~ list(NA_real_),
          Trait2 == "Fruit_type_1"             ~ list(99),
          Trait2 == "Dehiscence_1"             ~ list(2940),
          Trait2 == "Fruit_dryness_1"          ~ list(99),
          Trait2 == "Seed_volume_min"          ~ list(3402),
          Trait2 == "Seed_volume_max"          ~ list(3402),
          Trait2 == "Seed_volume_mean"         ~ list(3402),
          Trait2 == "Seed_volume_measured"     ~ list(3402),
          Trait2 == "Seed_mass_min"            ~ list(26),
          Trait2 == "Seed_mass_max"            ~ list(26),
          Trait2 == "Seed_mass_mean"           ~ list(26),
          Trait2 == "Seed_mass_measured"       ~ list(26),
          Trait2 == "Monocarpy"                ~ list(59),
          Trait2 == "Flower_colour"            ~ list(207),
          Trait2 == "Fruit_colour"             ~ list(585),
          Trait2 == "Inflorescence"            ~ list(2934),
          Trait2 == "Inflorescence_length_min" ~ list(2817),
          Trait2 == "Inflorescence_length_max" ~ list(2817),
          Trait2 == "Flower_length_min"        ~ list(913),
          Trait2 == "Flower_length_max"        ~ list(913),
          Trait2 == "Flower_width_min"         ~ list(NA_real_),
          Trait2 == "Flower_width_max"         ~ list(NA_real_),
          Trait2 == "Dispersal_syndrome_1"     ~ list(28),
          Trait2 == "Dispersal_syndrome_2"     ~ list(28),
          Trait2 == "Reproduction_sexual_1"    ~ list(213),
          Trait2 == "Reproduction_asexual_1"   ~ list(341),
          Trait2 == "Reproduction_asexual_2"   ~ list(356),
          Trait2 == "Pollination_syndrome_1"   ~ list(29),
          Trait2 == "Pollination_syndrome_2"   ~ list(29),
          Trait2 == "Pollination_syndrome_3"   ~ list(29),
          Trait2 == "Flowering_start"          ~ list(c(335, 2956)),
          Trait2 == "Flowering_end"            ~ list(335),
          Trait2 == "Fruiting_start"           ~ list(335),
          Trait2 == "Fruiting_end"             ~ list(335),
          Trait2 == "Seeds_per_fruit"          ~ list(335),
          # Physiological Traits
          Trait2 == "SLA_min"                  ~ list(c(3086, 3115:3117)),
          Trait2 == "SLA_max"                  ~ list(c(3086, 3115:3117)),
          Trait2 == "SLA_mean"                 ~ list(c(3086, 3115:3117)),
          Trait2 == "SLA_measured"             ~ list(c(3086, 3115:3117)),
          Trait2 == "Carnivory"                ~ list(c(42, 201)),
          Trait2 == "Leaf_form_1"              ~ list(154),
          Trait2 == "Leaf_margin_1"            ~ list(963),
          Trait2 == "Leaf_spines_1"            ~ list(677),
          Trait2 == "Leaf_thorns_1"            ~ list(677),
          Trait2 == "Leaf_arrangement"         ~ list(16),
          Trait2 == "Succulence"               ~ list(2),
          Trait2 == "Photosynthetic_pathway"   ~ list(22),
          Trait2 == "SSD_min"                  ~ list(4),
          Trait2 == "SSD_max"                  ~ list(4),
          Trait2 == "SSD_mean"                 ~ list(4),
          Trait2 == "SSD_measured"             ~ list(4),
          Trait2 == "Leaf_size_min"            ~ list(3108:3114),
          Trait2 == "Leaf_size_max"            ~ list(3108:3114),
          Trait2 == "Leaf_size_mean"           ~ list(3108:3114),
          Trait2 == "Leaf_size_measured"       ~ list(3108:3114),
          Trait2 == "Nitrogen_fix_1"           ~ list(8),
          Trait2 == "Leaf_length_min"          ~ list(c(144, 940)),
          Trait2 == "Leaf_length_max"          ~ list(c(144, 940)),
          Trait2 == "Leaf_length_mean"         ~ list(c(144, 940)),
          Trait2 == "Leaf_length_measured"     ~ list(c(144, 940)),
          Trait2 == "Leaf_width_min"           ~ list(145),
          Trait2 == "Leaf_width_max"           ~ list(145),
          Trait2 == "Leaf_width_mean"          ~ list(145),
          Trait2 == "Leaf_width_measured"      ~ list(145),
          Trait2 == "Leaf_thickness_min"       ~ list(46),
          Trait2 == "Leaf_thickness_max"       ~ list(46),
          Trait2 == "Leaf_thickness_mean"      ~ list(46),
          Trait2 == "Leaf_thickness_measured"  ~ list(46),
          Trait2 == "LDMC_min"                 ~ list(47),
          Trait2 == "LDMC_max"                 ~ list(47),
          Trait2 == "LDMC_mean"                ~ list(47),
          Trait2 == "LDMC_measured"            ~ list(47),
          # Genetical Traits
          Trait2 == "Chromosome_number"        ~ list(223),
          Trait2 == "Ploidy_level"             ~ list(222),
          # Ecological Traits
          Trait2 == "Elevational_range_min"    ~ list(1141),
          Trait2 == "Elevational_range_max"    ~ list(1141),
          Trait2 == "Elevational_range_mean"   ~ list(1141),
          Trait2 == "Bedrock_1"                ~ list(NA_real_),
          Trait2 == "Habitat_1"                ~ list(3096),
          Trait2 == "Mycorrhiza_1"             ~ list(7),
          Trait2 == "Mycorrhiza_family"        ~ list(NA_real_),
          Trait2 == "Mycorrhiza_species"       ~ list(NA_real_),
          Trait2 == "Defense"                  ~ list(345:346),
          TRUE ~ list(NA_real_),
        )
    ) %>%
      select(Trait2, trait_ids)
}

extract_gift_species_names = function(gift_names) {
  gift_names %>%
    select(genus, species_epithet, subtaxon, author) %>%
    mutate(full_name = paste(
      genus, species_epithet,
      ifelse(!is.na(author), author, ""),
      ifelse(!is.na(subtaxon), subtaxon, "")
    )) %>%
    pull(full_name) %>%
    unique()
}

extract_gift_names_with_traits = function(gift_traits_final, gift_names) {
  gift_names %>%
    semi_join(
      gift_traits_final %>%
        distinct(work_ID),
      by = "work_ID"
    )
}

harmonize_gift_glonaf = function(match_gift_tnrs, match_glonaf_tnrs) {
  match_gift_tnrs %>%
    distinct(name_init_gift        = Name_submitted,
             species_accepted_gift = Accepted_species) %>%
    inner_join(
      match_glonaf_tnrs %>%
        distinct(name_init_glonaf        = Name_submitted,
                 species_accepted_glonaf = Accepted_species),
      by = c(species_accepted_gift = "species_accepted_glonaf")) %>%
    filter(species_accepted_gift != "")
}

get_gift_traits_for_glonaf_species = function(
    gift_traits_final, gift_names_traits, harmonized_gift_glonaf,
    gift_traits_meta
) {
  gift_traits_final %>%
    inner_join(
      gift_names_traits %>%
        distinct(work_ID, species),
      by = "work_ID"
    ) %>%
    inner_join(
      harmonized_gift_glonaf %>%
        distinct(species = name_init_gift, species_accepted_gift),
      by = "species"
    ) %>%
    inner_join(
      gift_traits_meta %>%
        distinct(trait_ID = Lvl3, Trait2),
      by = "trait_ID"
    ) %>%
    distinct(species_accepted_gift, Trait2)
}

count_gift_species_per_trait = function(gift_glonaf_traits) {
  gift_glonaf_traits %>%
    dplyr::count(Trait2, sort = TRUE, name = "n_sp")
}

count_gift_trait_per_species = function(gift_glonaf_traits) {
  gift_glonaf_traits %>%
    dplyr::count(species_accepted_gift, sort = TRUE, name = "n_traits")
}

get_gift_trait_combinations = function(gift_glonaf_traits) {
  gift_glonaf_traits %>%
    dplyr::group_by(species = species_accepted_gift) %>%
    dplyr::summarise(trait_names = list(Trait2))
}

get_gift_top_trait_combinations = function(gift_glonaf_traits, aus_top_traits) {
  gift_glonaf_traits %>%
    semi_join(aus_top_traits, by = "Trait2") %>%
    get_gift_trait_combinations()
}
