simplify_bien_traits = function(bien_traits) {

  bien_traits |>
    distinct(scrubbed_species_binomial, trait_name)

}


get_bien_taxonomy = function(bien_traits) {

  bien_traits |>
    select(
      scrubbed_species_binomial,
      verbatim_family:scrubbed_species_binomial_with_morphospecies
    ) |>
    distinct()

}
