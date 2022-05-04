harmonize_austraits_glonaf = function(match_austraits_tnrs, match_glonaf_tnrs) {
  match_austraits_tnrs %>%
    distinct(name_init_austraits        = Name_submitted,
             species_accepted_austraits = Accepted_species) %>%
    inner_join(
      match_glonaf_tnrs %>%
        distinct(name_init_glonaf        = Name_submitted,
                 species_accepted_glonaf = Accepted_species),
      by = c(species_accepted_austraits = "species_accepted_glonaf")) %>%
  filter(species_accepted_austraits != "")
}

make_austraits_try_traits_correspond = function(austraits_try_convert) {
  austraits_try_convert %>%
    filter(!is.na(trait_name)) %>%
    select(aus_trait_name = trait_name,
           try_trait_name = `TRY name`,
           try_trait_id   = `TRY number`) %>%
    # Convert list of traits ids from character to actual list of integers
    mutate(
      try_trait_id = purrr::map(
        try_trait_id, function(trait_id_list) {
          if (!is.na(trait_id_list)) {
            # Split list by non words chararacters
            strsplit(trait_id_list, "\\W+") %>%
              .[[1]] %>%
              as.integer()
          } else {
            NA_integer_
          }
        }
      )
    )
}

make_austraits_bien_traits_correspond = function(
  aus_try_convert_df, bien_try_convert_df) {

  tibble::tribble(
    ~bien_trait_name, ~aus_trait_name,
    "plant flowering duration", "flowering_time",
    "plant fruiting duration",  "fruiting_time"
  )
}

get_austraits_traits_for_glonaf_species = function(
  austraits, harmonized_austraits_glonaf
) {
  austraits[["traits"]] %>%
    distinct(taxon_name, trait_name) %>%
    inner_join(harmonized_austraits_glonaf %>%
                 distinct(name_init_austraits, species_accepted_austraits),
               by = c(taxon_name = "name_init_austraits")) %>%
    distinct(species_accepted_austraits, trait_name) %>%
    arrange(species_accepted_austraits, trait_name) %>%
    filter(species_accepted_austraits != "")
}


count_austraits_species_per_trait = function(aus_traits) {
  aus_traits %>%
    dplyr::count(trait_name, sort = TRUE, name = "n_sp")
}

count_austraits_trait_per_species = function(aus_traits) {
  aus_traits %>%
    dplyr::count(species_accepted_austraits, sort = TRUE, name = "n_traits")
}

get_austraits_trait_combinations = function(aus_traits) {
  aus_traits %>%
    dplyr::group_by(species = species_accepted_austraits) %>%
    dplyr::summarise(trait_names = list(trait_name))
}

get_austraits_top_trait_combinations = function(aus_traits, aus_top_traits) {
  aus_traits %>%
    semi_join(aus_top_traits, by = "trait_name") %>%
    get_austraits_trait_combinations()
}

make_non_try_aus_traits_cat = function(consolidated_trait_names) {
  consolidated_trait_names %>%
    filter(
      is.na(try_trait_id) & is.na(bien_trait_name) & !is.na(aus_trait_name)
    ) %>%
    select(aus_trait_name) %>%
    mutate(
      trait_cat = case_when(
         aus_trait_name == "bark_mass_area"                        ~ "stem",
         aus_trait_name == "bark_water_content_per_dry_mass"       ~ "stem",
         aus_trait_name == "bark_water_content_per_saturated_mass" ~ "stem",
         aus_trait_name == "leaf_mass_to_stem_mass"                ~ "leaf",
         aus_trait_name == "leaf_water_content_per_area"           ~ "leaf",
         aus_trait_name == "stem_density"                          ~ "stem",
         aus_trait_name == "twig_area"                             ~ "stem",
         aus_trait_name == "accessory_cost_mass"                   ~ "stem",
         aus_trait_name == "seed_breadth"                          ~ "seed",
         aus_trait_name == "bark_ash_content_per_dry_mass"         ~ "stem",
         aus_trait_name == "bark_cellulose_per_dry_mass"           ~ "stem",
         aus_trait_name == "bark_lignin_per_dry_mass"              ~ "stem",
         aus_trait_name == "bark_tannin_per_dry_mass"              ~ "stem",
         aus_trait_name == "insoluable_protein_per_area"           ~ "leaf",
         aus_trait_name == "leaf_total_non-structural_carbohydrates_per_mass" ~
           "leaf",
         aus_trait_name == "leaf_total_non-structural_carbohydrates_per_area" ~
           "leaf",
         aus_trait_name == "stem_soluable_starch_per_mass"         ~ "stem",
         aus_trait_name == "stem_soluable_sugars_per_mass"         ~ "stem",
         aus_trait_name == "bark_Al_per_dry_mass"                  ~ "stem",
         aus_trait_name == "bark_B_per_dry_mass"                   ~ "stem",
         aus_trait_name == "bark_Na_per_dry_mass"                  ~ "stem",
         aus_trait_name == "cell_epidermis_Ca_per_fresh_mass"      ~ "leaf",
         aus_trait_name == "cell_epidermis_P_per_fresh_mass"       ~ "leaf",
         aus_trait_name == "cell_hypodermis_Ca_per_fresh_mass"     ~ "leaf",
         aus_trait_name == "cell_hypodermis_P_per_fresh_mass"      ~ "leaf",
         aus_trait_name == "cell_internal_parenchyma_Ca_per_fresh_mass" ~
           "leaf",
         aus_trait_name == "cell_internal_parenchyma_P_per_fresh_mass" ~ "leaf",
         aus_trait_name == "cell_palisade_mesophyll_Ca_per_fresh_mass" ~ "leaf",
         aus_trait_name == "cell_palisade_mesophyll_P_per_fresh_mass"  ~ "leaf",
         aus_trait_name == "cell_sclerenchyma_Ca_per_fresh_mass"       ~ "leaf",
         aus_trait_name == "cell_sclerenchyma_P_per_fresh_mass"        ~ "leaf",
         aus_trait_name == "cell_spongy_mesophyll_Ca_per_fresh_mass"   ~ "leaf",
         aus_trait_name == "cell_spongy_mesophyll_P_per_fresh_mass"    ~ "leaf",
         aus_trait_name == "cell_thylakoid_N_per_total_N"              ~ "leaf",
         aus_trait_name == "flower_N_per_dry_mass"                   ~ "flower",
         aus_trait_name == "fruit_Ca_per_dry_mass"                   ~ "seed",
         aus_trait_name == "fruit_K_per_dry_mass"                    ~ "seed",
         aus_trait_name == "fruit_Mg_per_dry_mass"                   ~ "seed",
         aus_trait_name == "fruit_N_per_dry_mass"                    ~ "seed",
         aus_trait_name == "fruit_P_per_dry_mass"                    ~ "seed",
         aus_trait_name == "fruit_S_per_dry_mass"                    ~ "seed",
         aus_trait_name == "senesced_leaf_B_per_dry_mass"            ~ "leaf",
         aus_trait_name == "resorption_leaf_N"                       ~ "leaf",
         aus_trait_name == "resorption_leaf_P"                       ~ "leaf",
         aus_trait_name == "seed_Ca_concentration"                   ~ "leaf",
         aus_trait_name == "seed_K_concentration"                    ~ "leaf",
         aus_trait_name == "seed_Mg_concentration"                   ~ "leaf",
         aus_trait_name == "seed_S_concentration"                    ~ "leaf",
         aus_trait_name == "calcicole_status"                  ~ "life_history",
         aus_trait_name == "competitive_stratum"               ~ "life_history",
         aus_trait_name == "establishment_light_environment_index" ~
           NA_character_,
         aus_trait_name == "flood_regime_classification" ~ NA_character_,
         aus_trait_name == "inundation_tolerance"        ~ NA_character_,
         aus_trait_name == "reproductive_light_environment_index" ~
           NA_character_,
         aus_trait_name == "snow_tolerance"              ~ NA_character_,
         aus_trait_name == "water_logging_tolerance"     ~ NA_character_,
         aus_trait_name == "dormancy_type" ~ "life_history",
         aus_trait_name == "fire_and_establishing"      ~ "life_history",
         aus_trait_name == "fire_cued_seeding"          ~ "life_history",
         aus_trait_name == "fire_flame_duration"        ~ "life_history",
         aus_trait_name == "fire_fuel_bed_bulk_density" ~ "life_history",
         aus_trait_name == "fire_fuel_comsumption"      ~ "life_history",
         aus_trait_name == "fire_rate_of_spread"        ~ "life_history",
         aus_trait_name == "fire_response_juvenile"     ~ "life_history",
         aus_trait_name == "fire_response_on_maturity"  ~ "life_history",
         aus_trait_name == "fire_smoulder_duration"     ~ "life_history",
         aus_trait_name == "fire_time_to_ignition"      ~ "life_history",
         aus_trait_name == "fire_total_burn_duration"   ~ "life_history",
         aus_trait_name == "seed_release"               ~ "seed",
         aus_trait_name == "time_from_fire_to_fruit"    ~ "life_history",
         aus_trait_name == "cotyledon_function"         ~ "leaf",
         aus_trait_name == "flower_structural_sex_type" ~ "flower",
         aus_trait_name == "reproductive_maturity"      ~ "life_history",
         aus_trait_name == "seedling_germination_location" ~ "seed",
         aus_trait_name == "growth_habit" ~ "life_history",
         aus_trait_name == "parasitic"    ~ "life_history",
         aus_trait_name == "epidermal_cell_density_abaxial"    ~ "leaf",
         aus_trait_name == "epidermal_cell_density_adaxial"    ~ "leaf",
         aus_trait_name == "epidermal_cell_density_both_sides" ~ "leaf",
         aus_trait_name == "lower_palisade_cell_thickness"     ~ "leaf",
         aus_trait_name == "palisade_cell_length"              ~ "leaf",
         aus_trait_name == "palisade_cell_width"               ~ "leaf",
         aus_trait_name == "palisade_layer_number"             ~ "leaf",
         aus_trait_name == "upper_cuticle_thickness"           ~ "leaf",
         aus_trait_name == "upper_palisade_cell_thickness"     ~ "leaf",
         aus_trait_name == "glaucous"       ~ "leaf",
         aus_trait_name == "leaf_curliness" ~ "leaf",
         aus_trait_name == "leaf_division"  ~ "leaf",
         aus_trait_name == "flower_androecium_structural_merism"     ~ "flower",
         aus_trait_name == "flower_androecium_structural_phyllotaxis"  ~
           "flower",
         aus_trait_name == "flower_androecium_structural_whorls_count" ~
           "flower",
         aus_trait_name == "flower_anther_attachment"           ~ "flower",
         aus_trait_name == "flower_anther_connective_extension" ~ "flower",
         aus_trait_name == "flower_anther_dehiscence"           ~ "flower",
         aus_trait_name == "flower_anther_orientation"          ~ "flower",
         aus_trait_name == "flower_fertile_stamens_count"       ~ "flower",
         aus_trait_name == "flower_filament"                    ~ "flower",
         aus_trait_name == "flower_filament_fusion"             ~ "flower",
         aus_trait_name == "flower_filament_fusion_to_inner_perianth" ~
           "flower",
         aus_trait_name == "flower_gynoecium_phyllotaxis"       ~ "flower",
         aus_trait_name == "flower_gynoecium_placentation"      ~ "flower",
         aus_trait_name == "flower_ovary_fusion"                ~ "flower",
         aus_trait_name == "flower_ovary_position"              ~ "flower",
         aus_trait_name == "flower_ovules_per_functional_carpel_count" ~
           "flower",
         aus_trait_name == "flower_perianth_differentiation"    ~ "flower",
         aus_trait_name == "flower_perianth_fusion"             ~ "flower",
         aus_trait_name == "flower_perianth_merism"             ~ "flower",
         aus_trait_name == "flower_perianth_parts_count"        ~ "flower",
         aus_trait_name == "flower_perianth_phyllotaxis"        ~ "flower",
         aus_trait_name == "flower_perianth_symmetry"           ~ "flower",
         aus_trait_name == "flower_perianth_whorls_count"       ~ "flower",
         aus_trait_name == "flower_pollen_aperture_shape"       ~ "flower",
         aus_trait_name == "flower_pollen_apertures_count"      ~ "flower",
         aus_trait_name == "flower_structural_carpels_count"    ~ "flower",
         aus_trait_name == "flower_style_differentiation"       ~ "flower",
         aus_trait_name == "flower_style_fusion"                ~ "flower",
         aus_trait_name == "fruit_breadth"                      ~ "seed",
         aus_trait_name == "fruit_wall_width"                   ~ "seed",
         aus_trait_name == "seed_mass_reserve"                  ~ "seed",
         aus_trait_name == "seed_texture"                       ~ "seed",
         aus_trait_name == "seed_longevity"                     ~ "seed",
         aus_trait_name == "root_morphology"                    ~ "root",
         aus_trait_name == "tap_root"                           ~ "root",
         aus_trait_name == "cotyledon_position"                 ~ "leaf",
         aus_trait_name == "cotyledon_type"                     ~ "leaf",
         aus_trait_name == "embryo_colour"                      ~ "seed",
         aus_trait_name == "hypocotyl_type"                     ~ "leaf",
         aus_trait_name == "seedling_first_leaf"                ~ "leaf",
         aus_trait_name == "seedling_first_node"                ~ "leaf",
         aus_trait_name == "vein_angle_secondary"               ~ "leaf",
         aus_trait_name == "vessel_density_leaves"              ~ "leaf",
         aus_trait_name == "vessel_diameter_hydraulic"          ~ "stem",
         aus_trait_name == "vessel_wall_fraction"               ~ "stem",
         aus_trait_name == "wood_tracheid_fraction"             ~ "stem",
         aus_trait_name == "vine_climbing_mechanism"            ~ "stem",
         aus_trait_name == "bark_morphology"                    ~ "stem",
         aus_trait_name == "ca"                                 ~ NA_character_,
         aus_trait_name == paste0("leaf_photosynthetic_phosphorus_use_",
                                  "efficiency_maximum") ~ "leaf",
         aus_trait_name == paste0("leaf_photosynthetic_phosphorus_use_",
                                  "efficiency_saturated") ~ "leaf",
         aus_trait_name == "water_use_efficiency_integrated"   ~ "life_history",
         aus_trait_name == "leaf_PRI"                          ~ "leaf",
         aus_trait_name == "leaf_reflectance"                  ~ "leaf",
         aus_trait_name == "leaf_reflectance_near_infrared"    ~ "leaf",
         aus_trait_name == "modified_NDVI"                     ~ "leaf",
         aus_trait_name == "photosynthetic_bark"               ~ "stem",
         aus_trait_name == "water_band_index"                  ~ "leaf",
         aus_trait_name == "fluorescence_Jmax_over_Vcmax"      ~ "leaf",
         aus_trait_name == "fluorescence_Jmax_per_mass"        ~ "leaf",
         aus_trait_name == "leaf_xylem_delta15N"               ~ "leaf",
         aus_trait_name == "root_xylem_delta15N"               ~ "root",
         aus_trait_name == "stem_water_delta18O"               ~ "stem",
         aus_trait_name == "leaf_relative_water_content_at_turgor_loss_point" ~
           "leaf",
         aus_trait_name == "leaf_relative_water_content_predawn" ~ "leaf",
         aus_trait_name == "modulus_of_elasticity_bark"          ~ "stem",
         aus_trait_name == "sapwood_specific_conductivity"       ~ "stem",
         aus_trait_name == "transverse_branch_area_specific_conductivity" ~
           "stem",
         TRUE ~ NA_character_
      ))
}
