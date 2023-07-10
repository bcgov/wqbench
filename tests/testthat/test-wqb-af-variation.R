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

test_that("No missing trophic_group are allowed ", {
  reps <- 1
  df <- data.frame(
    "trophic_group" = factor(rep(NA_character_, reps)),
    "species_number" = rep(1L, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  expect_error(
    wqb_af_variation(df),
    regexp = "must not have any missing values"
  )
})

test_that("No missing species_number are allowed ", {
  reps <- 1
  df <- data.frame(
    "trophic_group" = factor(rep("Fish", reps)),
    "species_number" = rep(NA_integer_, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  expect_error(
    wqb_af_variation(df),
    regexp = "must not have any missing values"
  )
})

test_that("Column is added", {
  reps <- 1L
  df <- data.frame(
    "trophic_group" = factor(c("Fish")),
    "species_number" = 1:reps,
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_variation(df)
  expect_true(
    "af_variation" %in% colnames(output)
  )
})

test_that("Single trophic level (fish) and single species gives af of 50", {
  reps <- 1L
  df <- data.frame(
    "trophic_group" = factor(c("Fish")),
    "species_number" = 1:reps,
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_variation(df)
  expect_equal(
    unique(output$af_variation),
    50L
  )
})

test_that("Single trophic level (plant) and single species gives af of 50", {
  reps <- 1L
  df <- data.frame(
    "trophic_group" = factor(c("Plant")),
    "species_number" = 1:reps,
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_variation(df)
  expect_equal(
    unique(output$af_variation),
    50L
  )
})

test_that("Single trophic level (algae) and single species gives af of 50", {
  reps <- 1L
  df <- data.frame(
    "trophic_group" = factor(c("Algae")),
    "species_number" = 1:reps,
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_variation(df)
  expect_equal(
    unique(output$af_variation),
    50L
  )
})

test_that("Single trophic level (invertebrate) and single species gives af of 50", {
  reps <- 1L
  df <- data.frame(
    "trophic_group" = factor(c("Invertebrate")),
    "species_number" = 1:reps,
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_variation(df)
  expect_equal(
    unique(output$af_variation),
    50L
  )
})

test_that("Single trophic level (amphibian) and single species gives af of 50", {
  reps <- 1L
  df <- data.frame(
    "trophic_group" = factor(c("Amphibian")),
    "species_number" = 1:reps,
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_variation(df)
  expect_equal(
    unique(output$af_variation),
    50L
  )
})

test_that("Single trophic level (amphibian) and two species gives af of 20", {
  reps <- 2L
  df <- data.frame(
    "trophic_group" = factor(rep("Amphibian", reps)),
    "species_number" = 1:reps,
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_variation(df)
  expect_equal(
    unique(output$af_variation),
    20L
  )
})

test_that("Single trophic level (amphibian) and three species gives af of 20", {
  reps <- 3L
  df <- data.frame(
    "trophic_group" = factor(rep("Amphibian", reps)),
    "species_number" = 1:reps,
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_variation(df)
  expect_equal(
    unique(output$af_variation),
    20L
  )
})

test_that("Single trophic level (fish) and three species gives af of 20", {
  reps <- 3L
  df <- data.frame(
    "trophic_group" = factor(rep("Fish", reps)),
    "species_number" = 1:reps,
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_variation(df)
  expect_equal(
    unique(output$af_variation),
    20L
  )
})

test_that("Single trophic level (fish) and three species gives af of 20", {
  reps <- 3L
  df <- data.frame(
    "trophic_group" = factor(rep("Fish", reps)),
    "species_number" = 1:reps,
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_variation(df)
  expect_equal(
    unique(output$af_variation),
    20L
  )
})

test_that("Single trophic level (fish) and four species gives af of 10", {
  reps <- 4L
  df <- data.frame(
    "trophic_group" = factor(rep("Fish", reps)),
    "species_number" = 1:reps,
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_variation(df)
  expect_equal(
    unique(output$af_variation),
    10L
  )
})

test_that("Single trophic level (fish) and five species gives af of 10", {
  reps <- 5L
  df <- data.frame(
    "trophic_group" = factor(rep("Fish", reps)),
    "species_number" = 1:reps,
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_variation(df)
  expect_equal(
    unique(output$af_variation),
    10L
  )
})

test_that("Single trophic level (fish) and six species gives af of 10", {
  reps <- 6L
  df <- data.frame(
    "trophic_group" = factor(rep("Fish", reps)),
    "species_number" = 1:reps,
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_variation(df)
  expect_equal(
    unique(output$af_variation),
    10L
  )
})

test_that("Single trophic level (fish) and seven species gives af of 5", {
  reps <- 7L
  df <- data.frame(
    "trophic_group" = factor(rep("Fish", reps)),
    "species_number" = 1:reps,
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_variation(df)
  expect_equal(
    unique(output$af_variation),
    5L
  )
})


test_that("Single trophic level (fish) and 20 species gives af of 5", {
  reps <- 20L
  df <- data.frame(
    "trophic_group" = factor(rep("Fish", reps)),
    "species_number" = 1:reps,
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_variation(df)
  expect_equal(
    unique(output$af_variation),
    5L
  )
})

test_that("Two trophic levels (fish, plant) and 2 species gives af of 10", {
  reps <- 2L
  df <- data.frame(
    "trophic_group" = factor(c(rep("Fish", 1L), rep("Plant", 1L))),
    "species_number" = 1:reps,
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_variation(df)
  expect_equal(
    unique(output$af_variation),
    10L
  )
})

test_that("Two trophic levels (fish, plant) and 3 species gives af of 10", {
  reps <- 3L
  df <- data.frame(
    "trophic_group" = factor(c(rep("Fish", 2L), rep("Plant", 1L))),
    "species_number" = 1:reps,
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_variation(df)
  expect_equal(
    unique(output$af_variation),
    10L
  )
})

test_that("Two trophic levels (fish, plant) and 4 species gives af of 5", {
  reps <- 4L
  df <- data.frame(
    "trophic_group" = factor(c(rep("Fish", 2L), rep("Plant", 2L))),
    "species_number" = 1:reps,
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_variation(df)
  expect_equal(
    unique(output$af_variation),
    5L
  )
})

test_that("Two trophic levels (fish, plant) and 5 species gives af of 5", {
  reps <- 5L
  df <- data.frame(
    "trophic_group" = factor(c(rep("Fish", 2L), rep("Plant", 3L))),
    "species_number" = 1:reps,
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_variation(df)
  expect_equal(
    unique(output$af_variation),
    5L
  )
})

test_that("Two trophic levels (fish, plant) and 6 species gives af of 5", {
  reps <- 6L
  df <- data.frame(
    "trophic_group" = factor(c(rep("Fish", 3L), rep("Plant", 3L))),
    "species_number" = 1:reps,
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_variation(df)
  expect_equal(
    unique(output$af_variation),
    5L
  )
})

test_that("Two trophic levels (fish, plant) and 7 species gives af of 3", {
  reps <- 7L
  df <- data.frame(
    "trophic_group" = factor(c(rep("Fish", 3L), rep("Plant", 4L))),
    "species_number" = 1:reps,
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_variation(df)
  expect_equal(
    unique(output$af_variation),
    3L
  )
})

test_that("Two trophic levels (fish, plant) and 20 species gives af of 3", {
  reps <- 20L
  df <- data.frame(
    "trophic_group" = factor(c(rep("Fish", 10L), rep("Plant", 10L))),
    "species_number" = 1:reps,
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_variation(df)
  expect_equal(
    unique(output$af_variation),
    3L
  )
})

test_that("Three trophic levels (fish, plant, algae) and 3 species gives af of 5", {
  reps <- 3L
  df <- data.frame(
    "trophic_group" = factor(c(rep("Fish", 1L), rep("Plant", 1L), rep("Algae", 1L))),
    "species_number" = 1:reps,
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_variation(df)
  expect_equal(
    unique(output$af_variation),
    5L
  )
})

test_that("Three trophic levels (fish, plant, algae) and 4 species gives af of 3", {
  reps <- 4L
  df <- data.frame(
    "trophic_group" = factor(c(rep("Fish", 2L), rep("Plant", 1L), rep("Algae", 1L))),
    "species_number" = 1:reps,
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_variation(df)
  expect_equal(
    unique(output$af_variation),
    3L
  )
})

test_that("Three trophic levels (fish, plant, algae) and 5 species gives af of 3", {
  reps <- 5L
  df <- data.frame(
    "trophic_group" = factor(c(rep("Fish", 2L), rep("Plant", 2L), rep("Algae", 1L))),
    "species_number" = 1:reps,
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_variation(df)
  expect_equal(
    unique(output$af_variation),
    3L
  )
})

test_that("Three trophic levels (fish, plant, algae) and 6 species gives af of 3", {
  reps <- 6L
  df <- data.frame(
    "trophic_group" = factor(c(rep("Fish", 2L), rep("Plant", 2L), rep("Algae", 2L))),
    "species_number" = 1:reps,
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_variation(df)
  expect_equal(
    unique(output$af_variation),
    3L
  )
})

test_that("Three trophic levels (fish, plant, algae) and 7 species gives af of 2", {
  reps <- 7L
  df <- data.frame(
    "trophic_group" = factor(c(rep("Fish", 3L), rep("Plant", 2L), rep("Algae", 2L))),
    "species_number" = 1:reps,
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_variation(df)
  expect_equal(
    unique(output$af_variation),
    2L
  )
})

test_that("Three trophic levels (fish, plant, algae) and 20 species gives af of 2", {
  reps <- 20L
  df <- data.frame(
    "trophic_group" = factor(c(rep("Fish", 10L), rep("Plant", 5L), rep("Algae", 5L))),
    "species_number" = 1:reps,
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_variation(df)
  expect_equal(
    unique(output$af_variation),
    2L
  )
})

test_that("Three trophic levels (amphibian, plant, algae) and 20 species gives af of 2", {
  reps <- 20L
  df <- data.frame(
    "trophic_group" = factor(c(rep("Amphibian", 10L), rep("Plant", 5L), rep("Algae", 5L))),
    "species_number" = 1:reps,
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_variation(df)
  expect_equal(
    unique(output$af_variation),
    2L
  )
})

test_that("Three trophic levels (amphibian, plant, invertebrate) and 20 species gives af of 2", {
  reps <- 20L
  df <- data.frame(
    "trophic_group" = factor(c(rep("Amphibian", 10L), rep("Plant", 5L), rep("Invertebrate", 5L))),
    "species_number" = 1:reps,
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_variation(df)
  expect_equal(
    unique(output$af_variation),
    2L
  )
})

test_that("Two trophic levels (amphibian, invertebrate) and 2 species gives af of 10", {
  reps <- 2L
  df <- data.frame(
    "trophic_group" = factor(c(rep("Amphibian", 1L), rep("Invertebrate", 1L))),
    "species_number" = 1:reps,
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_variation(df)
  expect_equal(
    unique(output$af_variation),
    10L
  )
})

test_that("Two trophic levels (amphibian, invertebrate) and 4 species gives af of 5", {
  reps <- 4L
  df <- data.frame(
    "trophic_group" = factor(c(rep("Amphibian", 3L), rep("Invertebrate", 1L))),
    "species_number" = 1:reps,
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_af_variation(df)
  expect_equal(
    unique(output$af_variation),
    5L
  )
})
