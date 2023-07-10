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

test_that("All four af columns are added", {
  reps <- 4L
  df <- data.frame(
    "species_number" = 1:reps,
    "trophic_group" = factor(rep("Invertebrate", reps)),
    "species_present_in_bc" = rep(TRUE, reps),
    "ecological_group" = factor(rep("Other", reps)),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af(df)
  expect_true(
    "af_bc_species" %in% colnames(output)
  )
  expect_true(
    "af_salmon" %in% colnames(output)
  )
  expect_true(
    "af_planktonic" %in% colnames(output)
  )
  expect_true(
    "af_variation" %in% colnames(output)
  )
})

test_that("Proper af's are applied test 1", {
  reps <- 4L
  df <- data.frame(
    "species_number" = 1:reps,
    "trophic_group" = factor(rep("Invertebrate", reps)),
    "species_present_in_bc" = rep(TRUE, reps),
    "ecological_group" = factor(rep("Other", reps)),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af(df)
  expect_equal(
    unique(output$af_bc_species),
    1L
  )
  expect_equal(
    unique(output$af_salmon),
    2L
  )
  expect_equal(
    unique(output$af_planktonic),
    2L
  )
  expect_equal(
    unique(output$af_variation),
    10L
  )
})

test_that("Proper af's are applied test 2", {
  reps <- 6L
  df <- data.frame(
    "species_number" = 1:reps,
    "trophic_group" = factor(rep("Invertebrate", reps)),
    "species_present_in_bc" = rep(TRUE, reps),
    "ecological_group" = factor(c(rep("Other", 2L), rep("Salmonid", 4L))),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af(df)
  expect_equal(
    unique(output$af_bc_species),
    1L
  )
  expect_equal(
    unique(output$af_salmon),
    1L
  )
  expect_equal(
    unique(output$af_planktonic),
    2L
  )
  expect_equal(
    unique(output$af_variation),
    10L
  )
})

test_that("Proper af's are applied test 3", {
  reps <- 6L
  df <- data.frame(
    "species_number" = 1:reps,
    "trophic_group" = factor(rep("Invertebrate", reps)),
    "species_present_in_bc" = rep(FALSE, reps),
    "ecological_group" = factor(c(rep("Other", 2L), rep("Salmonid", 2L), rep("Planktonic Invertebrate", 2L))),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af(df)
  expect_equal(
    unique(output$af_bc_species),
    3L
  )
  expect_equal(
    unique(output$af_salmon),
    1L
  )
  expect_equal(
    unique(output$af_planktonic),
    1L
  )
  expect_equal(
    unique(output$af_variation),
    10L
  )
})

test_that("Proper af's are applied test 4", {
  reps <- 8L
  df <- data.frame(
    "species_number" = 1:reps,
    "trophic_group" = factor(c(rep("Invertebrate", 3L), rep("Plant", 5L))),
    "species_present_in_bc" = c(rep(FALSE, 6L), rep(TRUE, 2L)),
    "ecological_group" = factor(c(rep("Other", 2L), rep("Salmonid", 4L), rep("Planktonic Invertebrate", 2L))),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af(df)
  expect_equal(
    unique(output$af_bc_species),
    2L
  )
  expect_equal(
    unique(output$af_salmon),
    1L
  )
  expect_equal(
    unique(output$af_planktonic),
    1L
  )
  expect_equal(
    unique(output$af_variation),
    3L
  )
})
