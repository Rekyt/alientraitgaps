simplify_austraits_traits = function(austraits) {

  austraits$traits |>
    distinct(taxon_name, trait_name)

}

merge_austraits_with_taxo = function(austraits_traits_simple, austraits_tnrs) {

  austraits_traits_simple |>
    inner_join(
      austraits_tnrs |>
        select(taxon_name = Name_submitted, Accepted_species),
      by = "taxon_name"
    ) |>
    distinct(Accepted_species, trait_name)

}
