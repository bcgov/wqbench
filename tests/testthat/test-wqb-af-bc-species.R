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

test_that("No missing species are allowed ", {
  reps <- 3
  df <- data.frame(
    "species_number" = c(1L, 2L, NA_integer_),
    "species_present_in_bc" = c(TRUE, FALSE, TRUE), 
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
    wqb_af_bc_species(df),
    regexp = "must not have any missing values"
  )
})

test_that("No missing bc species present are allowed ", {
  reps <- 3
  df <- data.frame(
    "species_number" = c(1L, 2L, 3L),
    "species_present_in_bc" = c(TRUE, FALSE, NA), 
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
    wqb_af_bc_species(df),
    regexp = "must not have any missing values"
  )
})

test_that("Column is added", {
  reps <- 5L
  df <- data.frame(
    "species_number" = 1L:reps,
    "species_present_in_bc" = c(rep(FALSE, 5L)), 
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
  output <- wqb_af_bc_species(df)
  expect_true(
    "af_bc_species" %in% colnames(output)
  )
})

test_that("af is 3 because there are 0 bc species present out of the 5 species", {
  reps <- 5L
  df <- data.frame(
    "species_number" = 1L:reps,
    "species_present_in_bc" = c(rep(FALSE, 5L)), 
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
  output <- wqb_af_bc_species(df)
  expect_equal(
    unique(output$af_bc_species),
    3L
  )
})

test_that("af is 3 because there are 0 bc species present out of the 20 species", {
  reps <- 20L
  df <- data.frame(
    "species_number" = 1L:reps,
    "species_present_in_bc" = c(rep(FALSE, 20L)), 
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
  output <- wqb_af_bc_species(df)
  expect_equal(
    unique(output$af_bc_species),
    3L
  )
})

test_that("af is 3 because there is only 1 bc species present out of the 5 species", {
  reps <- 5L
  df <- data.frame(
    "species_number" = 1L:reps,
    "species_present_in_bc" = c(rep(TRUE, 1L), rep(FALSE, 4L)), 
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
  output <- wqb_af_bc_species(df)
  expect_equal(
    unique(output$af_bc_species),
    3L
  )
})

test_that("af is 3 because there is only 1 bc species present out of the 20 species", {
  reps <- 20L
  df <- data.frame(
    "species_number" = 1L:reps,
    "species_present_in_bc" = c(rep(TRUE, 1L), rep(FALSE, 19L)), 
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
  output <- wqb_af_bc_species(df)
  expect_equal(
    unique(output$af_bc_species),
    3L
  )
})

test_that("af is 2 because there are 2 bc species present out of the 5 species", {
  reps <- 5L
  df <- data.frame(
    "species_number" = 1L:reps,
    "species_present_in_bc" = c(rep(TRUE, 2L), rep(FALSE, 3L)), 
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
  output <- wqb_af_bc_species(df)
  expect_equal(
    unique(output$af_bc_species),
    2L
  )
})

test_that("af is 2 because there is are 2 bc species present out of the 20 species", {
  reps <- 20L
  df <- data.frame(
    "species_number" = 1L:reps,
    "species_present_in_bc" = c(rep(TRUE, 2L), rep(FALSE, 18L)), 
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
  output <- wqb_af_bc_species(df)
  expect_equal(
    unique(output$af_bc_species),
    2L
  )
})

test_that("af is 2 because there are 3 bc species present out of the 5 species", {
  reps <- 5L
  df <- data.frame(
    "species_number" = 1L:reps,
    "species_present_in_bc" = c(rep(TRUE, 3L), rep(FALSE, 2L)), 
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
  output <- wqb_af_bc_species(df)
  expect_equal(
    unique(output$af_bc_species),
    2L
  )
})

test_that("af is 2 because there are 3 bc species present out of the 20 species", {
  reps <- 20L
  df <- data.frame(
    "species_number" = 1L:reps,
    "species_present_in_bc" = c(rep(TRUE, 3L), rep(FALSE, 17L)), 
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
  output <- wqb_af_bc_species(df)
  expect_equal(
    unique(output$af_bc_species),
    2L
  )
})

test_that("af is 1 because there are 4 or more bc species present out of the 5 species", {
  reps <- 5L
  df <- data.frame(
    "species_number" = 1L:reps,
    "species_present_in_bc" = c(rep(TRUE, 4L), rep(FALSE, 1L)), 
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
  output <- wqb_af_bc_species(df)
  expect_equal(
    unique(output$af_bc_species),
    1L
  )
})

test_that("af is 1 because there are 4 or more bc species present out of the 20 species", {
  reps <- 20L
  df <- data.frame(
    "species_number" = 1L:reps,
    "species_present_in_bc" = c(rep(TRUE, 4L), rep(FALSE, 16L)), 
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
  output <- wqb_af_bc_species(df)
  expect_equal(
    unique(output$af_bc_species),
    1L
  )
})
