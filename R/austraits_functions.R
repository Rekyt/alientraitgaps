simplify_austraits_traits = function(austraits) {

  austraits$traits |>
    distinct(taxon_name, trait_name)

}
