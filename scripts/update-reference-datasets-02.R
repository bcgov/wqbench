# Review is done in two steps since life stage codes depends on trophic group updates.
# This script updates the simple life stage coding.

rm(list = ls())
library(tidyverse)
library(chk)
library(daff)

# Setup -------------------------------------------------------------------

reviewed_folder <- 
  file.path(
    "~",
    "Poisson",
    "Data",
    "wqbench",
    format(Sys.Date(), "%Y"),
    "review",
    "completed"
  )

# render reports in the browser instead of the viewer pane
options(viewer = NULL)

# Life Stage --------------------------------------------------------------

life_stage_file_path <- system.file(
  "extdata/lifestage-codes.csv",
  package = "wqbench"
)

lifestage_std <- readr::read_csv(
  life_stage_file_path,
  show_col_types = FALSE
) 

reviewed_lifestage_fp <- list.files(
  path = file.path(reviewed_folder),
  pattern = "lifestage-code",
  full.names = TRUE
)

reviewed_lifestage_code <- 
  readr::read_csv(
    reviewed_lifestage_fp
  ) |>
  mutate(
    simple_lifestage = str_to_lower(simple_lifestage)
  ) |>
  rename(description_lifestage = description) |>
  filter(fish_amphibian_flag)

if (!vld_equal(sum(is.na(reviewed_lifestage_code$simple_lifestage[reviewed_lifestage_code$fish_amphibian_flag & !is.na(reviewed_lifestage_code$fish_amphibian_flag)])), 0)) {
  abort_chk("There should be no missing simple lifestage value, correct before proceeding")
}

if (!vld_subset(unique(reviewed_lifestage_code$simple_lifestage), c("els", "juvenile", "adult"))) {
  abort_chk("Ensure simple lifestage codes match allowed values")
}

if (!vld_equal(sum(duplicated(reviewed_lifestage_code)), 0)) {
  abort_chk("There should be no duplicate values")
}

reviewed_lifestage_code <- 
  reviewed_lifestage_code |>
  dplyr::select(-fish_amphibian_flag)

lifestage_daff <- daff::diff_data(
  lifestage_std, 
  reviewed_lifestage_code, 
  ordered = FALSE
)
daff::render_diff(
  lifestage_daff, 
  pretty = TRUE, 
  title = "Life Stage Codes"
)

if (FALSE) {
  # Only run this code if the html reports align with the requirements 
  # and all checks pass
  write_csv(
    reviewed_lifestage_code,
    "inst/extdata/lifestage-codes.csv",
  )
}