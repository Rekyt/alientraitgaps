library("dplyr")
library("targets")
tar_load(trait_knowledge_df)

tk = trait_knowledge_df %>%
  filter(
    !is.na(mean_hii_v2geo_mean), !is.na(gdp_mean_native),
    !is.na(gdp_mean_non_native), !is.na(mean_access_cities_2015_mean)
  ) %>%
  select(
    species, n_traits, simp_form, n_total, n_total_non_native, n_biomes,
    mean_hii_v2geo_mean, mean_hii_v2geo_sd, gdp_mean_native,
    gdp_mean_non_native, mean_access_cities_2015_mean
  ) %>%
  mutate(
    across(n_total:mean_access_cities_2015_mean, ~ as.numeric(scale(.x)))
  ) %>%
  rename(
    growth_form                   = simp_form,
    total_range_size              = n_total,
    non_native_range_size         = n_total_non_native,
    avg_human_influence_index     = mean_hii_v2geo_mean,
    sd_human_influence_index      = mean_hii_v2geo_sd,
    avg_gdp_over_native_range     = gdp_mean_native,
    avg_gdp_over_non_native_range = gdp_mean_non_native,
    avg_accessibility             = mean_access_cities_2015_mean
  ) %>%
  mutate(growth_form = case_when(
    growth_form == "herb/shrub" ~ "shrub",
    growth_form == "shrub/tree" ~ "tree",
    TRUE ~ growth_form
  )) %>%
  filter(growth_form != "unknown")

# Simple binomial shows Zero-One inflation plus totally non-normal residuals
# plus overdispersion
mod_bin = glm(
  cbind(n_traits, 2214 - n_traits) ~ growth_form + total_range_size + non_native_range_size +
    n_biomes + avg_human_influence_index + sd_human_influence_index +
    avg_gdp_over_native_range + avg_gdp_over_non_native_range +
    avg_accessibility,
  family    = binomial(),
  data      = tk
)

DHARMa::testResiduals(mod_bin)

# Quasi binomial model
# shows overdispersion
mod_quasibin = glm(
  cbind(n_traits, 2214 - n_traits) ~ growth_form + total_range_size + non_native_range_size +
    n_biomes + avg_human_influence_index + sd_human_influence_index +
    avg_gdp_over_native_range + avg_gdp_over_non_native_range +
    avg_accessibility,
  family    = quasibinomial(),
  data      = tk
)

performance::check_model(mod_quasibin)
performance::check_overdispersion(mod_quasibin)

# Try a Poisson model
# Clearly show zero-inflation and non-normality plus overdispersion
mod_poisson = glm(
  n_traits ~ growth_form + total_range_size + non_native_range_size +
    n_biomes + avg_human_influence_index + sd_human_influence_index +
    avg_gdp_over_native_range + avg_gdp_over_non_native_range +
    avg_accessibility,
  family    = poisson(),
  data      = tk
)

DHARMa::testResiduals(mod_poisson)
performance::check_model(mod_poisson)

# Check a quasi-poisson model
mod_quasipoi = glm(
  n_traits ~ growth_form + total_range_size + non_native_range_size +
    n_biomes + avg_human_influence_index + sd_human_influence_index +
    avg_gdp_over_native_range + avg_gdp_over_non_native_range +
    avg_accessibility,
  family    = quasipoisson(),
  data      = tk
)

performance::check_model(mod_quasipoi)

# Need to model zero-inflation
# Still clear overdispersion
mod_zipoi = glmmTMB::glmmTMB(
  n_traits ~ growth_form + total_range_size + non_native_range_size +
    n_biomes + avg_human_influence_index + sd_human_influence_index +
    avg_gdp_over_native_range + avg_gdp_over_non_native_range +
    avg_accessibility,
  family    = poisson(),
  ziformula = ~ 1,
  data      = tk
)

DHARMa::testResiduals(mod_zipoi)
performance::check_model(mod_zipoi)

# Poisson zero-inflated model with individual random effect to account for
# overdispersion
mod_zipoi_re = glmmTMB::glmmTMB(
  n_traits ~ growth_form + total_range_size + non_native_range_size +
    n_biomes + avg_human_influence_index + sd_human_influence_index +
    avg_gdp_over_native_range + avg_gdp_over_non_native_range +
    avg_accessibility + (1|species),
  family    = poisson(),
  ziformula = ~ growth_form + total_range_size,
  data      = tk
)

DHARMa::testResiduals(mod_zipoi_re)
performance::check_model(mod_zipoi_re)
plot(DHARMa::simulateResiduals(mod_zipoi_re, refit=T, n=99))

mod_zipoi_re_simp = glmmTMB::glmmTMB(
  n_traits ~ growth_form + total_range_size + n_biomes +
    avg_human_influence_index + avg_gdp_over_native_range +
    avg_accessibility + (1|species),
  family    = poisson(),
  ziformula = ~ growth_form + total_range_size,
  data      = tk
)
DHARMa::testResiduals(mod_zipoi_re_simp)

# Still overdispersed so trying a negative binomial model
#
mod_negbin = glmmTMB::glmmTMB(
  n_traits ~ growth_form + total_range_size + n_biomes +
    avg_human_influence_index + avg_gdp_over_native_range +
    avg_accessibility + (1|species),
  family    = glmmTMB::nbinom1(),
  ziformula = ~ 1,
  data      = tk
)
DHARMa::testDispersion(mod_negbin)

DHARMa::testResiduals(mod_negbin)
performance::check_model(mod_negbin)

mod_negbin2 = glmmTMB::glmmTMB(
  n_traits ~ growth_form + total_range_size + n_biomes +
    avg_human_influence_index + avg_gdp_over_native_range +
    avg_accessibility + (1|species),
  family    = glmmTMB::nbinom2(),
  ziformula = ~ 1,
  data      = tk
)

