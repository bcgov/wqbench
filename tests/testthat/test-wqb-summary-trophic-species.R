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

test_that("error when missing value in trophic group column", {
  reps <- 1
  df <- data.frame(
    "trophic_group" = factor(rep(NA_character_, reps)),
    "af_variation" = rep(NA, reps),
    "af_salmon" =  rep(NA, reps),
    "af_planktonic" =  rep(NA, reps),
    "af_bc_species" =  rep(NA, reps),
    "species_number" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  expect_error(
    wqb_summary_trophic_species(df),
    regexp = "`data\\$trophic_group` must not have any missing values"
  )
})

test_that("error with character trophic groups", {
  reps <- 1
  df <- data.frame(
    "trophic_group" = rep("Fish", reps),
    "af_variation" = rep(NA, reps),
    "af_salmon" =  rep(NA, reps),
    "af_planktonic" =  rep(NA, reps),
    "af_bc_species" =  rep(NA, reps),
    "species_number" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  expect_error(
    wqb_summary_trophic_species(df),
    regexp = "`data\\$trophic_group` must inherit from S3 class 'factor'"
  )
})

test_that("number of species outputs correct counts", {
  reps <- 1
  df <- data.frame(
    "trophic_group" = factor(rep("Fish", reps), levels = c("Fish", "Plant", "Algae", "Invertebrate", "Amphibian")),
    "af_variation" = rep(NA, reps),
    "af_salmon" =  rep(NA, reps),
    "af_planktonic" =  rep(NA, reps),
    "af_bc_species" =  rep(NA, reps),
    "species_number" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_summary_trophic_species(df)
  expect_equal(
    output$`Number of Species`,
    c(1L, 0L, 0L, 0L, 0L)
  )
  expect_equal(
    as.character(output$`Trophic Group`),
    c("Fish", "Plant", "Algae", "Invertebrate", "Amphibian")
  )
})

test_that("number of species outputs correct counts with fish and amphibian", {
  reps <- 15L
  df <- data.frame(
    "trophic_group" = factor(
      c(rep("Fish", 5L), rep("Amphibian", 10L)), 
      levels = c("Fish", "Plant", "Algae", "Invertebrate", "Amphibian")
    ),
    "af_variation" = rep(NA, reps),
    "af_salmon" =  rep(NA, reps),
    "af_planktonic" =  rep(NA, reps),
    "af_bc_species" =  rep(NA, reps),
    "species_number" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_summary_trophic_species(df)
  expect_equal(
    output$`Number of Species`,
    c(5L, 0L, 0L, 0L, 10L)
  )
})

test_that("number of species outputs correct counts with all groups", {
  reps <- 27L
  df <- data.frame(
    "trophic_group" = factor(
      c(rep("Fish", 5L), rep("Amphibian", 10L), rep("Plant", 2L), rep("Algae", 3L), rep("Invertebrate", 7L)), 
      levels = c("Fish", "Plant", "Algae", "Invertebrate", "Amphibian")
    ),
    "af_variation" = rep(NA, reps),
    "af_salmon" =  rep(NA, reps),
    "af_planktonic" =  rep(NA, reps),
    "af_bc_species" =  rep(NA, reps),
    "species_number" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_summary_trophic_species(df)
  expect_equal(
    output$`Number of Species`,
    c(5L, 2L, 3L, 7L, 10L)
  )
})
