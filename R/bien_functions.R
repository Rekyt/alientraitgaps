count_bien_traits_per_species = function(harmonized_try_glonaf) {
  harmonized_try_glonaf %>%
    pull(species_accepted_try) %>%
    unique() %>%
    BIEN::BIEN_trait_traits_per_species()
}

get_bien_traits = function(harmonized_try_glonaf) {
  harmonized_try_glonaf %>%
    pull(species_accepted_try) %>%
    unique() %>%
    BIEN::BIEN_trait_species()
}

make_bien_try_correspond = function(bien_traits) {
  bien_traits %>%
    filter(!is.na(trait_name)) %>%
    mutate(trait_ids = case_when(
      # DBH
      trait_name == "diameter at breast height (1.3 m)" ~ list(21),
      trait_name == "flower color"                      ~ list(207),
      trait_name == "flower pollination syndrome"       ~ list(29),
      trait_name == "fruit type"                        ~ list(99),
      trait_name == "inflorescence length"              ~ list(2817),

      ## Leafs
      trait_name == "leaf area"                         ~ list(c(3108:3113)),
      trait_name == "leaf area per leaf dry mass" ~ list(c(3086, 3115:3117)),
      trait_name == "leaf carbon content per leaf area" ~ list(570),
      trait_name == "leaf carbon content per leaf dry mass" ~ list(13),
      # leaf C/N ratio
      trait_name == "leaf carbon content per leaf nitrogen content" ~ list(146),
      trait_name == "leaf compoundness" ~ list(17),
      trait_name == "leaf dry mass"     ~ list(55),
      # LDMC
      trait_name == "leaf dry mass per leaf fresh mass" ~ list(47),
      trait_name == "leaf fresh mass"                   ~ list(163),
      # Unsure? TRY trait is leaf texture (physical strength)
      trait_name == "Leaf lamina fracture toughness"             ~ list(2),
      trait_name == "leaf life span"                             ~ list(12),
      trait_name == "leaf nitrogen content per leaf area"        ~ list(50),
      trait_name == "leaf nitrogen content per leaf dry mass"    ~ list(14),
      trait_name == "leaf phosphorus content per leaf area"      ~ list(51),
      trait_name == "leaf phosphorus content per leaf dry mass"  ~ list(15),
      trait_name == "leaf photosynthetic rate per leaf area"     ~ list(53),
      trait_name == "leaf photosynthetic rate per leaf dry mass" ~ list(40),
      # NO leaf relative growth rate in TRY?
      trait_name == "leaf relative growth rate" ~ list(NA_real_),
      trait_name == "leaf stomatal conductance for H2O per leaf area" ~ list(45),
      # No difference in TRY between H2O or other conductance
      trait_name == "leaf stomatal conductance per leaf area" ~ list(45),
      trait_name == "leaf thickness"                          ~ list(46),

      ## Maximums
      # NOT maximum (what's the difference between maximum and longest?!)
      trait_name == "longest whole plant longevity" ~ list(59),
      # NOT maximum 918: Fruit length; 27: Seed length
      trait_name == "maximum fruit length" ~ list(918),
      # NOT maximum 144: Leaf length;
      # 940: Leaf length excluding petiole (leaf lamina length)
      trait_name == "maximum leaf length" ~ list(c(144, 940)),
      # NOT maximum
      trait_name == "maximum leaf width" ~ list(145),
      # NOT maximum 3106: Plant height vegetative; 3107: Plant height generative
      trait_name == "maximum whole plant height" ~ list(3106),
      # NOT maximum
      trait_name == "maximum whole plant longevity" ~ list(59),

      ## Minimums
      # NOT minimum
      trait_name == "minimum fruit length" ~ list(27),
      # NOT minimum 144: Leaf length;
      # 940: Leaf length excluding petiole (leaf lamina length)
      trait_name == "minimum leaf length" ~ list(c(144, 940)),
      # NOT minimum
      trait_name == "minimum leaf width" ~ list(145),
      # NOT minimum 3106: Plant height vegetative; 3107: Plant height generative
      trait_name == "minimum whole plant height" ~ list(3106),

      ## Phenology
      # 335: Plant reproductive phenology timing?
      # 155: Plant ontogeny: age of maturity (first flowering)
      # 2956:	Flower onset of flowering (first flowering date,
      #       flowering beginning)
      # Flowering traits are in TOP-Thesaurus and in BIOLFLOR but not in TRY
      trait_name == "plant flowering begin"    ~ list(2956),
      trait_name == "plant flowering end"      ~ list(NA_real_),
      trait_name == "plant flowering duration" ~ list(NA_real_),

      ## Roots
      # 457:	Coarse root dry mass per ground area
      # 798: Root dry mass per ground area
      # 1795: Fine root dry mass per ground area
      trait_name == "root dry mass" ~ list(c(457, 798, 1795)),

      ## Seeds
      trait_name == "seed length" ~ list(27),
      trait_name == "seed mass"   ~ list(26),

      ## Stem
      # 128:	Plant biomass and allometry: Stem dry mass per plant
      trait_name == "stem dry mass"             ~ list(128),
      trait_name == "stem relative growth rate" ~ list(NA_real_),
      trait_name == "stem wood density"         ~ list(4),

      ## Vessels
      # 170:	Stem conduit cross-sectional area (vessels and tracheids)
      trait_name == "vessel lumen area" ~ list(170),
      # Not number but density
      # 169:	Stem conduit density (vessels and tracheids)
      trait_name == "vessel number" ~ list(169),

      ## Whole Plant
      trait_name == "whole plant dispersal syndrome" ~ list(28),
      # 42:	Plant growth form
      # 3401:	Plant growth form detailed consolidated
      # 3400:	Plant growth form simple consolidated
      trait_name == "whole plant growth form"           ~ list(c(42, 3400)),
      trait_name == "whole plant growth form diversity" ~ list(3401),
      trait_name == "whole plant height"                ~ list(3106),
      # 155:	Plant ontogeny: age of maturity (first flowering)
      trait_name == "whole plant primary juvenile period length" ~ list(155),
      # 347: Plant mating system
      # see also 208:	Species reproduction type
      trait_name == "whole plant sexual system" ~ list(347),
      # Should check meaning of trait
      trait_name == "whole plant vegetative phenology" ~ list(1251),
      trait_name == "whole plant woodiness" ~ list(38),

      ## Otherwise
      TRUE ~ list(NA_real_)
    )) %>%
    as_tibble()
}

count_bien_species_per_trait = function(glonaf_bien_traits_count) {
  glonaf_bien_traits_count %>%
    distinct(species = scrubbed_species_binomial, trait_name) %>%
    filter(!is.na(trait_name)) %>%
    group_by(trait_name) %>%
    summarise(n_sp = n()) %>%
    arrange(desc(n_sp))
}


make_bien_trait_category = function(bien_traits) {
  bien_traits %>%
    filter(!is.na(trait_name)) %>%
    mutate(trait_cat = case_when(
      trait_name == "diameter at breast height (1.3 m)"             ~ "stem",
      trait_name == "flower color"                                  ~ "flower",
      trait_name == "flower pollination syndrome"                   ~ "flower",
      trait_name == "fruit type"                                    ~ "seed",
      trait_name == "inflorescence length"                          ~ "shoot",

      ## Leafs
      trait_name == "leaf area"                                     ~ "leaf",
      trait_name == "leaf area per leaf dry mass"                   ~ "leaf",
      trait_name == "leaf carbon content per leaf area"             ~ "leaf",
      trait_name == "leaf carbon content per leaf dry mass"         ~ "leaf",
      # leaf C/N ratio
      trait_name == "leaf carbon content per leaf nitrogen content" ~ "leaf",
      trait_name == "leaf compoundness"                             ~ "leaf",
      trait_name == "leaf dry mass"                                 ~ "leaf",
      # LDMC
      trait_name == "leaf dry mass per leaf fresh mass"             ~ "leaf",
      trait_name == "leaf fresh mass"                               ~ "leaf",
      # Unsure? TRY trait is leaf texture (physical strength)
      trait_name == "Leaf lamina fracture toughness"                ~ "leaf",
      trait_name == "leaf life span"                                ~ "leaf",
      trait_name == "leaf nitrogen content per leaf area"           ~ "leaf",
      trait_name == "leaf nitrogen content per leaf dry mass"       ~ "leaf",
      trait_name == "leaf phosphorus content per leaf area"         ~ "leaf",
      trait_name == "leaf phosphorus content per leaf dry mass"     ~ "leaf",
      trait_name == "leaf photosynthetic rate per leaf area"        ~ "leaf",
      trait_name == "leaf photosynthetic rate per leaf dry mass"    ~ "leaf",
      # NO leaf relative growth rate in TRY?
      trait_name == "leaf relative growth rate" ~ "leaf",
      trait_name == "leaf stomatal conductance for H2O per leaf area" ~ "leaf",
      # No difference in TRY between H2O or other conductance
      trait_name == "leaf stomatal conductance per leaf area"       ~ "leaf",
      trait_name == "leaf thickness"                                ~ "leaf",

      ## Maximums
      trait_name == "longest whole plant longevity" ~ "life_history",
      trait_name == "maximum fruit length"          ~ "seed",
      trait_name == "maximum leaf length"           ~ "leaf",
      trait_name == "maximum leaf width"            ~ "leaf",
      trait_name == "maximum whole plant height"    ~ "height",
      trait_name == "maximum whole plant longevity" ~ "life_history",

      ## Minimums
      trait_name == "minimum fruit length"       ~ "seed",
      trait_name == "minimum leaf length"        ~ "leaf",
      trait_name == "minimum leaf width"         ~ "leaf",
      trait_name == "minimum whole plant height" ~ "height",

      ## Phenology
      trait_name == "plant flowering begin"    ~ "flower",
      trait_name == "plant flowering end"      ~ "flower",
      trait_name == "plant flowering duration" ~ "flower",

      ## Roots
      trait_name == "root dry mass" ~ "root",

      ## Seeds
      trait_name == "seed length" ~ "seed",
      trait_name == "seed mass"   ~ "seed",

      ## Stem
      trait_name == "stem dry mass"             ~ "stem",
      trait_name == "stem relative growth rate" ~ "stem",
      trait_name == "stem wood density"         ~ "stem",

      ## Vessels
      trait_name == "vessel lumen area" ~ "stem",
      trait_name == "vessel number"     ~ "stem",

      ## Whole Plant
      trait_name == "whole plant dispersal syndrome"              ~ "seed",
      trait_name == "whole plant growth form"                     ~ "life_history",
      trait_name == "whole plant growth form diversity"           ~ "life_history",
      trait_name == "whole plant height"                          ~ "height",
      trait_name == "whole plant primary juvenile period length"  ~ "life_history",
      trait_name == "whole plant sexual system"                   ~ "life_history",
      trait_name == "whole plant vegetative phenology"            ~ "life_history",
      trait_name == "whole plant woodiness"                       ~ "life_history",

      ## Otherwise
      TRUE ~ list(NA_real_)
    )) %>%
    as_tibble()
}
