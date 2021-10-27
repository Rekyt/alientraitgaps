rank_species_trait_number = function(
  glonaf_bien_traits_count, try_total_number_trait,
  glonaf_try_traits_available, harmonized_try_glonaf) {

  list(
    bien = glonaf_bien_traits_count %>%
      select(-count) %>%
      group_by(species = scrubbed_species_binomial) %>%
      summarise(trait_number = n()),

    try_full = try_total_number_trait %>%
      select(species = species_accepted_try, trait_number),

    try_extract = glonaf_try_traits_available %>%
      filter(!is.na(TraitID)) %>%
      distinct(species = species_accepted_try, TraitID) %>%
      group_by(species) %>%
      summarise(trait_number = n())
  ) %>%
    bind_rows(.id = "trait_db") %>%
    group_by(trait_db) %>%
    full_join(
      harmonized_try_glonaf %>%
        distinct(species = species_accepted_try) %>%
        pull(species) %>%
        expand.grid(trait_db = c("bien", "try_full", "try_extract")) %>%
        rename(species = Var1),
      by = c("trait_db", "species")
    ) %>%
    mutate(trait_number = ifelse(is.na(trait_number), 0, trait_number)) %>%
    arrange(desc(trait_number)) %>%
    mutate(species_trait_number_rank = row_number()) %>%
    ungroup()
}

count_tuples_of_traits = function(list_of_lists, number_of_traits) {
  list_of_lists %>%
    purrr::keep(~ length(.x) > (number_of_traits - 1)) %>%
    purrr::map(~ combn(.x, number_of_traits, simplify = FALSE)) %>%
    purrr::modify_depth(2, sort) %>%
    purrr::flatten() %>%
    purrr::map_chr(paste, collapse = " x ") %>%
    table() %>%
    sort(decreasing = TRUE) %>%
    tibble::enframe(name = "trait_combination", value = "n_species") %>%
    mutate(trait_number = number_of_traits)
}
