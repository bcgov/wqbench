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

test_that("error when missing value in af variation column", {
  reps <- 1
  df <- data.frame(
    "af_variation" = rep(NA_integer_, reps),
    "af_salmon" = rep(1L, reps),
    "af_planktonic" = rep(1L, reps),
    "af_bc_species" = rep(1L, reps),
    "species_number" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  expect_error(
    wqb_summary_af(df),
    regexp = "`data\\$af_variation` must not have any missing values"
  )
})

test_that("error when missing value in af salmon column", {
  reps <- 1
  df <- data.frame(
    "af_variation" = rep(1L, reps),
    "af_salmon" = rep(NA_integer_, reps),
    "af_planktonic" = rep(1L, reps),
    "af_bc_species" = rep(1L, reps),
    "species_number" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  expect_error(
    wqb_summary_af(df),
    regexp = "`data\\$af_salmon` must not have any missing values"
  )
})

test_that("error when missing value in af planktonic column", {
  reps <- 1
  df <- data.frame(
    "af_variation" = rep(1L, reps),
    "af_salmon" = rep(1L, reps),
    "af_planktonic" = rep(NA_integer_, reps),
    "af_bc_species" = rep(1L, reps),
    "species_number" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  expect_error(
    wqb_summary_af(df),
    regexp = "`data\\$af_planktonic` must not have any missing values"
  )
})

test_that("error when missing value in af bc species column", {
  reps <- 1
  df <- data.frame(
    "af_variation" = rep(1L, reps),
    "af_salmon" = rep(1L, reps),
    "af_planktonic" = rep(1L, reps),
    "af_bc_species" = rep(NA_integer_, reps),
    "species_number" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  expect_error(
    wqb_summary_af(df),
    regexp = "`data\\$af_bc_species` must not have any missing values"
  )
})

test_that("combines ecological af factors properly", {
  reps <- 1
  df <- data.frame(
    "af_variation" = rep(1L, reps),
    "af_salmon" = rep(4L, reps),
    "af_planktonic" = rep(5L, reps),
    "af_bc_species" = rep(1L, reps),
    "species_number" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_summary_af(df)
  expect_equal(
    output$`Assessment Factor`,
    c(1L, 20L, 1L)
  )
})

test_that("check column names ", {
  reps <- 1
  df <- data.frame(
    "af_variation" = rep(1L, reps),
    "af_salmon" = rep(4L, reps),
    "af_planktonic" = rep(5L, reps),
    "af_bc_species" = rep(1L, reps),
    "species_number" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_summary_af(df)
  expect_equal(
    colnames(output),
    c("Consideration", "Assessment Factor", "Description")
  )
})

test_that("check consideration values", {
  reps <- 1
  df <- data.frame(
    "af_variation" = rep(1L, reps),
    "af_salmon" = rep(4L, reps),
    "af_planktonic" = rep(5L, reps),
    "af_bc_species" = rep(1L, reps),
    "species_number" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_summary_af(df)
  expect_equal(
    output$Consideration,
    c("Species variation factor", "Ecological assessment factor", "B.C. species")
  )
})

test_that("check description values", {
  reps <- 1
  df <- data.frame(
    "af_variation" = rep(1L, reps),
    "af_salmon" = rep(4L, reps),
    "af_planktonic" = rep(5L, reps),
    "af_bc_species" = rep(1L, reps),
    "species_number" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_summary_af(df)
  expect_equal(
    output$Description,
    c(
      "Accounts for uncertainty due to limited species and trophic coverage",
      "Accounts for uncertainty when missing data on planktonic invertebrates and/or salmonids",
      "Accounts for uncertainty of not having representation of B.C. species"
    )
  )
})

test_that("check af values match up", {
  reps <- 1
  df <- data.frame(
    "af_variation" = rep(3L, reps),
    "af_salmon" = rep(4L, reps),
    "af_planktonic" = rep(5L, reps),
    "af_bc_species" = rep(1L, reps),
    "species_number" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_summary_af(df)
  expect_equal(
    output$`Assessment Factor`,
    c(3L, 20L, 1L)
  )
})


test_that("check af values match up when multiple rows present ", {
  reps <- 10
  df <- data.frame(
    "af_variation" = rep(3L, reps),
    "af_salmon" = rep(4L, reps),
    "af_planktonic" = rep(5L, reps),
    "af_bc_species" = rep(1L, reps),
    "species_number" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_summary_af(df)
  expect_equal(
    output$`Assessment Factor`,
    c(3L, 20L, 1L)
  )
})
