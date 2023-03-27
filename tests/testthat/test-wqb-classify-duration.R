



df <- tibble::tribble(
  ~ecological_group, ~duration_mean_std, ~simple_lifestage, ~expected_duration_class,
  "algae",            23,                NA_character_,     "acute"
)


test_that("algae less then 24 hours is acute", {
  df <- tibble::tribble(
    ~ecological_group, ~duration_mean_std, ~simple_lifestage, ~expected_duration_class, ~ecological_group_class,
    "algae",            23,                NA_character_,     "acute", "other",
    "algae",            10,                NA_character_,     "acute", "other",
    "algae",            0,                NA_character_,      "acute", "other"
  ) |>
   dplyr::mutate(ecological_group = factor(ecological_group))
  
  #output <- wqb_classify_duration(df)
  
})
