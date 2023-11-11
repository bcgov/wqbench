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

test_that("method column can't have missing values", {
  reps <- 4L
  df <- data.frame(
    "sp_aggre_conc_mg.L" = rep(1, reps),
    "method" = rep(NA_character_, reps),
    "species_number" = rep(NA, reps),
    "trophic_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps)
  )
  expect_error(
    wqb_method_ssd(df),
    "must not have any missing values"
  )
})

test_that("conc column can't have missing values", {
  reps <- 4L
  df <- data.frame(
    "sp_aggre_conc_mg.L" = rep(NA_real_, reps),
    "method" = rep("SSD", reps),
    "species_number" = rep(NA, reps),
    "trophic_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps)
  )
  expect_error(
    wqb_method_ssd(df),
    "must not have any missing values"
  )
})

test_that("method column can only have SSD", {
  reps <- 1L
  df <- data.frame(
    "sp_aggre_conc_mg.L" = rep(1, reps),
    "method" = rep("Deterministic", reps),
    "species_number" = rep(NA, reps),
    "trophic_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps)
  )
  expect_error(
    wqb_method_ssd(df),
    "must match 'SSD', not 'Deterministic'"
  )
})

test_that("fit errors if conc values are missing", {
  reps <- 6L
  df <- data.frame(
    "sp_aggre_conc_mg.L" = c(1, 2, NA_real_, 3, 4, NA_real_),
    "method" = rep("SSD", reps),
    "species_number" = rep(NA, reps),
    "trophic_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps)
  )
  expect_error(
    wqb_ssd_fit(df),
    "`data` has 2 rows with effectively missing values in 'sp_aggre_conc_mg\\.L'"
  )
})

test_that("values match up", {
  skip_if_testing_quick()

  set.seed(10)
  reps <- 6L
  df <- data.frame(
    "sp_aggre_conc_mg.L" = c(1, 2, 1.5, 3, 4, 2.5),
    "method" = rep("SSD", reps),
    "species_number" = rep(NA, reps),
    "trophic_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps)
  )
  output <- wqb_method_ssd(df, wqb_ssd_fit(df))
  expect_s3_class(output, "tbl_df")
  expect_equal(
    signif(output$ctv_est_mg.L, 3),
    0.950
  )
  expect_equal(
    signif(output$ctv_lcl_mg.L, 3),
    0.528
  )
  expect_equal(
    signif(output$ctv_ucl_mg.L, 3),
    1.90
  )
})

test_that("values differ with more bootstraps", {
  skip_if_testing_quick()
  
  set.seed(10)
  reps <- 6L
  df <- data.frame(
    "sp_aggre_conc_mg.L" = c(1, 2, 1.5, 3, 4, 2.5),
    "method" = rep("SSD", reps),
    "species_number" = rep(NA, reps),
    "trophic_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps)
  )
  output <- wqb_method_ssd(df, wqb_ssd_fit(df), nboot = 10000)
  expect_s3_class(output, "tbl_df")
  expect_equal(
    signif(output$ctv_est_mg.L, 3),
    0.950
  )
  expect_equal(
    signif(output$ctv_lcl_mg.L, 3),
    0.526
  )
  expect_equal(
    signif(output$ctv_ucl_mg.L, 3),
    1.89
  )
})
