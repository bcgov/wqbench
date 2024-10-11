# Copyright 2024 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# First round of updates for concentration conversions, duration conversions,
# and trophic groups.

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
  ) |>
  mutate(
    conc_conversion_flag = as.logical(conc_conversion_flag)
  )

reviewed_conc_std_fp <- list.files(
  path = file.path(reviewed_folder),
  pattern = "concentration-conversion",
  full.names = TRUE
)

reviewed_conc_std <-
  readr::read_csv(
    reviewed_conc_std_fp,
    col_types = cols(
      code = col_character(),
      description = col_character(),
      conc_conversion_flag = col_logical(),
      conc_conversion_value_multiplier = col_double(),
      conc_conversion_unit = col_character()
    )
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

if (!vld_subset(unique(reviewed_conc_std$conc_conversion_unit), c("mg/L", "ppm", NA_character_))) {
  abort_chk("Concentration units can only include ppm and mg/L, correct before proceeding")
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
  col_types = cols(
    code = col_character(),
    description = col_character(),
    duration_units_to_keep = col_logical(),
    duration_value_multiplier_to_hours = col_double(),
    comments = col_character()
  )
)

reviewed_duration_std <- list.files(
  path = file.path(reviewed_folder),
  pattern = "duration-conversion",
  full.names = TRUE
)

reviewed_duration_std <- readr::read_csv(
  reviewed_duration_std
) |>
  mutate(
    duration_units_to_keep = as.logical(duration_units_to_keep)
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
    tax_order = str_to_title(tax_order),
    trophic_group = str_to_title(trophic_group),
    ecological_group = str_to_title(ecological_group)
  )

# Extract those that were not flagged to exclude
add_trophic_groups <- reviewed_trophic_groups |>
  filter(is.na(exclude_from_db)) |>
  select(-exclude_from_db) |>
  rename(order = tax_order)

if (!vld_equal(sum(is.na(add_trophic_groups$trophic_group)), 0)) {
  abort_chk("There should be no missing trophic groups, correct before proceeding")
}

if (!vld_equal(sum(is.na(add_trophic_groups$ecological_group)), 0)) {
  abort_chk("There should be no missing trophic groups, correct before proceeding")
}

if (!vld_subset(unique(add_trophic_groups$trophic_group), c("Invertebrate", "Algae", "Amphibian", "Plant", "Bacteria", "Fish"))) {
  abort_chk("Ensure trophic groups match allowed values")
}

if (!vld_subset(unique(add_trophic_groups$ecological_group), c("Planktonic Invertebrate", "Other", "Salmonid"))) {
  abort_chk("Ensure ecological groups match allowed values")
}

# Combine new trophic groups with existing
new_trophic_groups <- bind_rows(trophic_group_file_path_std, add_trophic_groups) |>
  distinct()

if (!vld_equal(sum(duplicated(new_trophic_groups)), 0)) {
  abort_chk("There should be no duplicate values")
}

trophic_daff <- daff::diff_data(
  trophic_group_file_path_std,
  new_trophic_groups,
  ordered = FALSE
)
daff::render_diff(
  trophic_daff,
  pretty = TRUE,
  title = "Trophic Groups"
)

## Update exclusions -------------------------------------------------------
exclude_taxa <- read_csv("inst/extdata/exclude_taxa.csv", na = character())

add_exclude_taxa <- reviewed_trophic_groups |>
  filter(!is.na(exclude_from_db)) |>
  select(-trophic_group, -ecological_group, -exclude_from_db)

new_exclude_taxa <- bind_rows(exclude_taxa, add_exclude_taxa) |>
  distinct()

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
    new_trophic_groups,
    "inst/extdata/trophic-group.csv",
  )

  write_csv(
    new_exclude_taxa,
    "inst/extdata/exclude_taxa.csv",
  )
}
