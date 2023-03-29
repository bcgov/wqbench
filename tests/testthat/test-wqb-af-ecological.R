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

test_that("No missing ecological_group are allowed ", {
  reps <- 3
  df <- data.frame(
    "ecological_group" = factor(rep(NA, reps)),
    "species_number" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  expect_error(
    wqb_af_ecological(df),
    regexp = "must not have any missing values"
  )
})

test_that("both af columns are added", {
  reps <- 3
  df <- data.frame(
    "ecological_group" = factor("Other", "Other", "Other"),
    "species_number" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_ecological(df)
  expect_true(
    "af_salmon" %in% colnames(output)
  )
  expect_true(
    "af_planktonic" %in% colnames(output)
  )
})

test_that("af_salmon is 2 and af_plankton is 2 because only other group present", {
  reps <- 3
  df <- data.frame(
    "ecological_group" = factor("Other", "Other", "Other"),
    "species_number" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_ecological(df)
  expect_equal(
    unique(output$af_salmon),
    2L
  )
  expect_equal(
    unique(output$af_planktonic),
    2L
  )
})

test_that("af_salmon is 1 and af_plankton is 2 because salmonid group present", {
  reps <- 3
  df <- data.frame(
    "ecological_group" = factor(c("Salmonid", "Other", "Other")),
    "species_number" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_ecological(df)
  expect_equal(
    unique(output$af_salmon),
    1L
  )
  expect_equal(
    unique(output$af_planktonic),
    2L
  )
})

test_that("af_salmon is 1 and af_plankton is 2 and only salmonid group present", {
  reps <- 3
  df <- data.frame(
    "ecological_group" = factor(c("Salmonid", "Salmonid", "Salmonid")),
    "species_number" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_ecological(df)
  expect_equal(
    unique(output$af_salmon),
    1L
  )
  expect_equal(
    unique(output$af_planktonic),
    2L
  )
})

test_that("af_salmon is 1 and af_plankton is 1 with salmonid and planktonic group present", {
  reps <- 2
  df <- data.frame(
    "ecological_group" = factor(c("Salmonid", "Planktonic Invertebrate")),
    "species_number" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_ecological(df)
  expect_equal(
    unique(output$af_salmon),
    1L
  )
  expect_equal(
    unique(output$af_planktonic),
    1L
  )
})

test_that("af_salmon is 1 and af_plankton is 1 with all groups present", {
  reps <- 3
  df <- data.frame(
    "ecological_group" = factor(c("Salmonid", "Planktonic Invertebrate", "Other")),
    "species_number" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_ecological(df)
  expect_equal(
    unique(output$af_salmon),
    1L
  )
  expect_equal(
    unique(output$af_planktonic),
    1L
  )
})

test_that("af_salmon is 2 and af_plankton is 1 since planktonic group present", {
  reps <- 3
  df <- data.frame(
    "ecological_group" = factor(c("Other", "Planktonic Invertebrate", "Other")),
    "species_number" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_ecological(df)
  expect_equal(
    unique(output$af_salmon),
    2L
  )
  expect_equal(
    unique(output$af_planktonic),
    1L
  )
})

test_that("mutliples of the groups provide expected results of 1 and 1 for af", {
  reps <- 7
  df <- data.frame(
    "ecological_group" = factor(c("Other", "Planktonic Invertebrate", "Other", "Planktonic Invertebrate", "Salmonid", "Salmonid", "Other")),
    "species_number" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_ecological(df)
  expect_equal(
    unique(output$af_salmon),
    1L
  )
  expect_equal(
    unique(output$af_planktonic),
    1L
  )
})
