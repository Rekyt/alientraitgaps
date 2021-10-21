get_all_tr8_traits = function(harmonized_try_glonaf) {
  harmonized_try_glonaf %>%
    pull(species_accepted_try) %>%
    TR8::tr8(
      # Get all possible traits
      download_list = TR8::available_tr8 %>%
        filter(db != "AMF") %>%  # Remove AMF because of bugs with traits
        pull(short_code),
      # Always online queries
      allow_persistent = FALSE
    )
}
