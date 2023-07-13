# Copyright 2023 Province of British Columbia
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

test_that("cas must be present and not missing in data", {
  reps <- 1
  df <- data.frame(
    "cas" = rep(NA_character_, reps),
    "endpoint" = rep(NA, reps),
    "duration_class" = rep(NA, reps),
    "trophic_group" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "lifestage" = rep(NA, reps),
    "duration_hrs" = rep(NA, reps),
    "effect_conc_std_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps),
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "present_in_bc_wqg" = rep(NA, reps),
    "species_number" = rep(NA, reps),
    "download_date" = rep(NA, reps),
    "version" = rep(NA, reps)
  )
  expect_error(
    wqb_filter_chemical(df, "129909906"),
    regexp = "must not have any missing values"
  )
})

test_that("cas_num must be a string", {
  reps <- 1
  df <- data.frame(
    "cas" = rep("1", reps),
    "endpoint" = rep(NA, reps),
    "duration_class" = rep(NA, reps),
    "trophic_group" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "lifestage" = rep(NA, reps),
    "duration_hrs" = rep(NA, reps),
    "effect_conc_std_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps),
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "present_in_bc_wqg" = rep(NA, reps),
    "species_number" = rep(NA, reps),
    "download_date" = rep(NA, reps),
    "version" = rep(NA, reps)
  )
  expect_error(
    wqb_filter_chemical(df, 129909906),
    regexp = "`cas_num` must be a string \\(non-missing character scalar\\)"
  )
})

test_that("filters to a single chemcial", {
  reps <- 10L
  df <- data.frame(
    "cas" = c(rep("1", 5L), rep("2", 5L)),
    "endpoint" = rep(NA, reps),
    "duration_class" = rep(NA, reps),
    "trophic_group" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "lifestage" = rep(NA, reps),
    "duration_hrs" = rep(NA, reps),
    "effect_conc_std_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps),
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "present_in_bc_wqg" = rep(NA, reps),
    "species_number" = rep(NA, reps),
    "download_date" = rep(NA, reps),
    "version" = rep(NA, reps)
  )
  output <- wqb_filter_chemical(df, "1")
  expect_equal(
    nrow(output),
    c(5L)
  )
})

test_that("No rows selected when chemical not in data set", {
  reps <- 10L
  df <- data.frame(
    "cas" = c(rep("1", 5L), rep("2", 5L)),
    "endpoint" = rep(NA, reps),
    "duration_class" = rep(NA, reps),
    "trophic_group" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "lifestage" = rep(NA, reps),
    "duration_hrs" = rep(NA, reps),
    "effect_conc_std_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps),
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "present_in_bc_wqg" = rep(NA, reps),
    "species_number" = rep(NA, reps),
    "download_date" = rep(NA, reps),
    "version" = rep(NA, reps)
  )
  output <- wqb_filter_chemical(df, "3")
  expect_equal(
    nrow(output),
    c(0L)
  )
})
