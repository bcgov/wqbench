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

# Concentration Conversion ------------------------------------------------

concentration_std_file_path <- system.file(
  "extdata/concentration-conversion.csv",
  package = "wqbench"
)

concentration_std <- readr::read_csv(
  concentration_std_file_path,
  show_col_types = FALSE
) |>
  select(
    code, description, conc_conversion_flag, conc_conversion_value_multiplier, 
    conc_conversion_unit
  )

reviewed_conc_std_fp <- list.files(
  path = file.path(reviewed_folder),
  pattern = "concentration-conversion",
  full.names = TRUE
)

reviewed_conc_std <- readr::read_csv(
  reviewed_conc_std_fp
)

if (!vld_equal(sum(is.na(reviewed_conc_std$conc_conversion_flag)), 0)) {
  abort_chk("There should be no missing conc_conversion_flag values, correct before proceeding")
}

if (!vld_equal(sum(is.na(reviewed_conc_std$conc_conversion_value_multiplier[reviewed_conc_std$conc_conversion_flag == 1])), 0)) {
  abort_chk("There can't be a flag of 1 with no multipler, correct before proceeding")
}

if (!vld_equal(sum(is.na(reviewed_conc_std$conc_conversion_unit[reviewed_conc_std$conc_conversion_flag == 1])), 0)) {
  abort_chk("There can't be a flag of 1 with no conversion unit, correct before proceeding")
}

if (!vld_equal(sum(duplicated(reviewed_conc_std)), 0)) {
  abort_chk("There should be no duplicate values")
}

concentration_daff <- daff::diff_data(
  concentration_std, 
  reviewed_conc_std, 
  ordered = FALSE
)
daff::render_diff(
  concentration_daff, 
  pretty = TRUE, 
  title = "Concentration Conversion"
)

# Duration Conversion -----------------------------------------------------

duration_std_file_path <- system.file(
  "extdata/duration-conversion.csv",
  package = "wqbench"
)

duration_std <- readr::read_csv(
  duration_std_file_path,
  show_col_types = FALSE
)

reviewed_duration_std <- list.files(
  path = file.path(reviewed_folder),
  pattern = "duration-conversion",
  full.names = TRUE
)

reviewed_duration_std <- readr::read_csv(
  reviewed_duration_std
)

if (!vld_equal(sum(is.na(reviewed_duration_std$duration_units_to_keep)), 0)) {
  abort_chk("There should be no missing duration_units_to_keep values, correct before proceeding")
}

if (!vld_equal(sum(is.na(reviewed_duration_std$duration_value_multiplier_to_hours[reviewed_duration_std$duration_units_to_keep == 1])), 0)) {
  abort_chk("There can't be a flag of 1 with no multipler, correct before proceeding")
}

if (!vld_equal(sum(is.na(reviewed_duration_std$comments[reviewed_duration_std$duration_units_to_keep == 1])), 0)) {
  abort_chk("There can't be a flag of 1 with no comment, correct before proceeding")
}

if (!vld_equal(sum(duplicated(reviewed_duration_std)), 0)) {
  abort_chk("There should be no duplicate values")
}

duration_daff <- daff::diff_data(
  duration_std, 
  reviewed_duration_std, 
  ordered = FALSE
)
daff::render_diff(
  duration_daff, 
  pretty = TRUE, 
  title = "Duration Conversion"
)

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

reviewed_lifestage_code <- readr::read_csv(
  reviewed_lifestage_fp
) |>
  mutate(
    simple_lifestage = str_to_lower(simple_lifestage)
  )

if (!vld_equal(sum(is.na(reviewed_lifestage_code$simple_lifestage)), 0)) {
  abort_chk("There should be no missing simple lifestage value, correct before proceeding")
}

if (!vld_equal(sum(duplicated(reviewed_lifestage_code)), 0)) {
  abort_chk("There should be no duplicate values")
}


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

# Trophic Groups ----------------------------------------------------------

trophic_group_file_path <- system.file(
  "extdata/trophic-group.csv",
  package = "wqbench"
)

trophic_group_file_path_std <- readr::read_csv(
  trophic_group_file_path,
  show_col_types = FALSE
)

reviewed_trophic_group_fp <- list.files(
  path = file.path(reviewed_folder),
  pattern = "trophic-group",
  full.names = TRUE
)

reviewed_trophic_groups <- readr::read_csv(
  reviewed_trophic_group_fp
) |>
  mutate(
    class = str_to_title(class),
    order = str_to_title(order),
    trophic_group = str_to_title(trophic_group),
    ecological_group = str_to_title(ecological_group)
  )

if (!vld_equal(sum(is.na(reviewed_trophic_groups$trophic_group)), 0)) {
  abort_chk("There should be no missing trophic groups, correct before proceeding")
}

if (!vld_equal(sum(is.na(reviewed_trophic_groups$ecological_group)), 0)) {
  abort_chk("There should be no missing trophic groups, correct before proceeding")
}

if (!vld_subset(unique(reviewed_trophic_groups$trophic_group), c("Invertebrate", "Algae", "Amphibian", "Plant", "Bacteria", "Fish"))) {
  abort_chk("Ensure trophic groups match allowed values")
}

if (!vld_subset(unique(reviewed_trophic_groups$ecological_group), c("Planktonic Invertebrate", "Other", "Salmonid"))) {
  abort_chk("Ensure ecological groups match allowed values")
}

if (!vld_equal(sum(duplicated(reviewed_trophic_groups)), 0)) {
  abort_chk("There should be no duplicate values")
}

trophic_daff <- daff::diff_data(
  trophic_group_file_path_std, 
  reviewed_trophic_groups, 
  ordered = FALSE
)
daff::render_diff(
  trophic_daff, 
  pretty = TRUE, 
  title = "Trophic Groups"
)

## Write new reference file ------------------------------------------------

if (FALSE) {
  # Only run this code if the html reports align with the requirements 
  # and all checks pass
  write_csv(
    reviewed_duration_std,
    "inst/extdata/duration-conversion.csv",
  )
  
  write_csv(
    reviewed_conc_std,
    "inst/extdata/concentration-conversion.csv",
  )
  
  write_csv(
    reviewed_conc_std,
    "inst/extdata/lifestage-codes.csv",
  )
  
  write_csv(
    reviewed_trophic_groups,
    "inst/extdata/trophic-group.csv",
  )
  
}
