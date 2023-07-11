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

# General ----
test_that("check message output", {
  reps <- 1
  df <- data.frame(
    "endpoint" = rep("LOEC", reps),
    "duration_class" = rep("acute", reps),
    "trophic_group" = factor(rep("fish", reps)),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  expect_message(
    wqb_standardize_effect(df),
    regexp = "Standardize type of effect"
  )
})

test_that("error if endpoint has any missing values", {
  reps <- 1
  df <- data.frame(
    "endpoint" = c("LOEC", NA_character_),
    "duration_class" = rep("acute", reps),
    "trophic_group" = factor(rep("fish"), reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
    wqb_standardize_effect(df),
    regexp = "must not have any missing values"
  )
})

test_that("error if duration class has any missing values", {
  reps <- 2
  df <- data.frame(
    "endpoint" = c("LOEC", "LOEC"),
    "duration_class" = c("acute", NA_character_),
    "trophic_group" = factor(rep("fish"), reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
    wqb_standardize_effect(df),
    regexp = "must not have any missing values"
  )
})

test_that("error if trophic group has any missing values", {
  reps <- 2
  df <- data.frame(
    "endpoint" = c("LOEC", "LOEC"),
    "duration_class" = c("acute", "acute"),
    "trophic_group" = factor(c("fish", NA_character_)),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
    wqb_standardize_effect(df),
    regexp = "must not have any missing values"
  )
})

# Acute ----
test_that("check acute and endpoint LOEC", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("LOEC", reps),
    "duration_class" = rep("acute", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(10L, 10L, 10L, 10L, 5L)
  )
})

test_that("check acute and endpoint LOEL", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("LOEL", reps),
    "duration_class" = rep("acute", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(10L, 10L, 10L, 10L, 5L)
  )
})

test_that("check acute and endpoint MCIG", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("MCIG", reps),
    "duration_class" = rep("acute", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(10L, 10L, 10L, 10L, 5L)
  )
})

test_that("check acute and endpoint EC20", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("EC20", reps),
    "duration_class" = rep("acute", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(10L, 10L, 10L, 10L, 5L)
  )
})

test_that("check acute and endpoint EC30", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("EC30", reps),
    "duration_class" = rep("acute", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(10L, 10L, 10L, 10L, 5L)
  )
})

test_that("check acute and endpoint IC20", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("IC20", reps),
    "duration_class" = rep("acute", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(10L, 10L, 10L, 10L, 5L)
  )
})

test_that("check acute and endpoint IC30", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("IC30", reps),
    "duration_class" = rep("acute", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(10L, 10L, 10L, 10L, 5L)
  )
})

test_that("check acute and endpoint LC20", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("LC20", reps),
    "duration_class" = rep("acute", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(10L, 10L, 10L, 10L, 5L)
  )
})

test_that("check acute and endpoint LC30", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("LC30", reps),
    "duration_class" = rep("acute", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(10L, 10L, 10L, 10L, 5L)
  )
})

test_that("check acute and endpoint EC19", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("EC19", reps),
    "duration_class" = rep("acute", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(5L, 5L, 5L, 5L, 5L)
  )
})

test_that("check acute and endpoint EC10", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("EC10", reps),
    "duration_class" = rep("acute", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(5L, 5L, 5L, 5L, 5L)
  )
})

test_that("check acute and endpoint IC19", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("IC19", reps),
    "duration_class" = rep("acute", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(5L, 5L, 5L, 5L, 5L)
  )
})

test_that("check acute and endpoint IC10", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("IC10", reps),
    "duration_class" = rep("acute", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(5L, 5L, 5L, 5L, 5L)
  )
})

test_that("check acute and endpoint LC19", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("LC19", reps),
    "duration_class" = rep("acute", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(5L, 5L, 5L, 5L, 5L)
  )
})

test_that("check acute and endpoint LC10", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("LC10", reps),
    "duration_class" = rep("acute", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(5L, 5L, 5L, 5L, 5L)
  )
})

test_that("check acute and endpoint NOEC", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("NOEC", reps),
    "duration_class" = rep("acute", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(5L, 5L, 5L, 5L, 5L)
  )
})

test_that("check acute and endpoint NOEL", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("NOEL", reps),
    "duration_class" = rep("acute", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(5L, 5L, 5L, 5L, 5L)
  )
})

test_that("check acute and endpoint MATC", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("MATC", reps),
    "duration_class" = rep("acute", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(5L, 5L, 5L, 5L, 5L)
  )
})

# Chronic ----

test_that("check chronic and endpoint LOEC", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("LOEC", reps),
    "duration_class" = rep("chronic", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(5L, 5L, 5L, 5L, 5L)
  )
})

test_that("check chronic and endpoint LOEL", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("LOEL", reps),
    "duration_class" = rep("chronic", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(5L, 5L, 5L, 5L, 5L)
  )
})

test_that("check chronic and endpoint MCIG", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("MCIG", reps),
    "duration_class" = rep("chronic", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(5L, 5L, 5L, 5L, 5L)
  )
})

test_that("check chronic and endpoint EC20", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("EC20", reps),
    "duration_class" = rep("chronic", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(5L, 5L, 5L, 5L, 5L)
  )
})

test_that("check chronic and endpoint EC30", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("EC30", reps),
    "duration_class" = rep("chronic", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(5L, 5L, 5L, 5L, 5L)
  )
})

test_that("check chronic and endpoint IC20", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("IC20", reps),
    "duration_class" = rep("chronic", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(5L, 5L, 5L, 5L, 5L)
  )
})

test_that("check chronic and endpoint IC30", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("IC30", reps),
    "duration_class" = rep("chronic", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(5L, 5L, 5L, 5L, 5L)
  )
})

test_that("check chronic and endpoint LC20", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("LC20", reps),
    "duration_class" = rep("chronic", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(5L, 5L, 5L, 5L, 5L)
  )
})

test_that("check chronic and endpoint LC30", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("LC30", reps),
    "duration_class" = rep("chronic", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(5L, 5L, 5L, 5L, 5L)
  )
})

test_that("check chronic and endpoint EC19", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("EC19", reps),
    "duration_class" = rep("chronic", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(1L, 1L, 1L, 1L, 1L)
  )
})

test_that("check chronic and endpoint EC10", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("EC10", reps),
    "duration_class" = rep("chronic", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(1L, 1L, 1L, 1L, 1L)
  )
})

test_that("check chronic and endpoint IC19", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("IC19", reps),
    "duration_class" = rep("chronic", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(1L, 1L, 1L, 1L, 1L)
  )
})

test_that("check chronic and endpoint IC10", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("IC10", reps),
    "duration_class" = rep("chronic", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(1L, 1L, 1L, 1L, 1L)
  )
})

test_that("check chronic and endpoint LC19", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("LC19", reps),
    "duration_class" = rep("chronic", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(1L, 1L, 1L, 1L, 1L)
  )
})

test_that("check chronic and endpoint LC10", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("LC10", reps),
    "duration_class" = rep("chronic", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(1L, 1L, 1L, 1L, 1L)
  )
})

test_that("check chronic and endpoint NOEC", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("NOEC", reps),
    "duration_class" = rep("chronic", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(1L, 1L, 1L, 1L, 1L)
  )
})

test_that("check chronic and endpoint NOEL", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("NOEL", reps),
    "duration_class" = rep("chronic", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(1L, 1L, 1L, 1L, 1L)
  )
})

test_that("check chronic and endpoint MATC", {
  reps <- 5
  df <- data.frame(
    "endpoint" = rep("MATC", reps),
    "duration_class" = rep("chronic", reps),
    "trophic_group" = factor(c("fish", "amphibian", "invertebrate", "plant", "algae")),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
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
  output <- wqb_standardize_effect(df, quiet = TRUE)
  expect_equal(
    output$acr,
    # fish, frog, invert, plant, algae
    c(1L, 1L, 1L, 1L, 1L)
  )
})
