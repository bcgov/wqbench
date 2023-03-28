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
test_that("The single species is aggregated to a single row", {
  reps <- 4
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = rep(1, reps),
    "endpoint" = rep("NOEC", reps),
    "method" = rep("SSD", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    nrow(output),
    1L
  )
})

test_that("The single species is aggregated to a single row with multiple lifestages", {
  reps <- 4
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = c("Egg", "Egg", "Larva", "Larva"),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = rep(1, reps),
    "endpoint" = rep("NOEC", reps),
    "method" = rep("SSD", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    nrow(output),
    1L
  )
})

test_that("The single species is aggregated to a single row with multiple lifestages and effects", {
  reps <- 4
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = c("Egg", "Egg", "Larva", "Larva"),
    "effect" = c("Mortality", "Growth", "Mortality", "Growth"),
    "effect_conc_std_mg.L" = rep(1, reps),
    "endpoint" = rep("NOEC", reps),
    "method" = rep("SSD", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    nrow(output),
    1L
  )
})

test_that("Two species is aggregated to a single row each with multiple lifestages and effects", {
  reps <- 4
  df <- data.frame(
    "species_number" = c(1L, 1L, 2L, 2L),
    "lifestage" = c("Egg", "Egg", "Larva", "Larva"),
    "effect" = c("Mortality", "Growth", "Mortality", "Growth"),
    "effect_conc_std_mg.L" = rep(1, reps),
    "endpoint" = rep("NOEC", reps),
    "method" = rep("SSD", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    nrow(output),
    2L
  )
})

# SSD ----
test_that("Highest priorty group (with EC05) selected with ssd method", {
  reps <- 17
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(10, 2, 2, 2, 2, 3, 4, 4, 5, 5, 5, 6, 6, 7, 7, 8, 8),
    "endpoint" = c("EC05", "EC11", "EC20", "IC11", "IC20", "MATC", "NOEC", "NOEL", "LOEC", "LOEL", "MCIG", "EC21", "IC21", "LC5", "LC19", "LC20", "LC21"),
    "method" = rep("SSD", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    10
  )
})

test_that("Highest priorty group (with EC10) selected with ssd method", {
  reps <- 17
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(10, 2, 2, 2, 2, 3, 4, 4, 5, 5, 5, 6, 6, 7, 7, 8, 8),
    "endpoint" = c("EC10", "EC11", "EC20", "IC11", "IC20", "MATC", "NOEC", "NOEL", "LOEC", "LOEL", "MCIG", "EC21", "IC21", "LC5", "LC19", "LC20", "LC21"),
    "method" = rep("SSD", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    10
  )
})

test_that("Highest priorty group (with IC10) selected with ssd method", {
  reps <- 17
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(10, 2, 2, 2, 2, 3, 4, 4, 5, 5, 5, 6, 6, 7, 7, 8, 8),
    "endpoint" = c("IC10", "EC11", "EC20", "IC11", "IC20", "MATC", "NOEC", "NOEL", "LOEC", "LOEL", "MCIG", "EC21", "IC21", "LC5", "LC19", "LC20", "LC21"),
    "method" = rep("SSD", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    10
  )
})

test_that("Highest priorty group (with IC05) selected with ssd method", {
  reps <- 17
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(10, 2, 2, 2, 2, 3, 4, 4, 5, 5, 5, 6, 6, 7, 7, 8, 8),
    "endpoint" = c("IC05", "EC11", "EC20", "IC11", "IC20", "MATC", "NOEC", "NOEL", "LOEC", "LOEL", "MCIG", "EC21", "IC21", "LC5", "LC19", "LC20", "LC21"),
    "method" = rep("SSD", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    10
  )
})

test_that("Second highest priorty group (with EC11) selected with ssd method", {
  reps <- 13
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(10, 3, 4, 4, 5, 5, 5, 6, 6, 7, 7, 8, 8),
    "endpoint" = c("EC11", "MATC", "NOEC", "NOEL", "LOEC", "LOEL", "MCIG", "EC21", "IC21", "LC5", "LC19", "LC20", "LC21"),
    "method" = rep("SSD", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    10
  )
})

test_that("Second highest priorty group (with EC15) selected with ssd method", {
  reps <- 13
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(10, 3, 4, 4, 5, 5, 5, 6, 6, 7, 7, 8, 8),
    "endpoint" = c("EC15", "MATC", "NOEC", "NOEL", "LOEC", "LOEL", "MCIG", "EC21", "IC21", "LC5", "LC19", "LC20", "LC21"),
    "method" = rep("SSD", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    10
  )
})

test_that("Second highest priorty group (with EC20) selected with ssd method", {
  reps <- 13
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(10, 3, 4, 4, 5, 5, 5, 6, 6, 7, 7, 8, 8),
    "endpoint" = c("EC20", "MATC", "NOEC", "NOEL", "LOEC", "LOEL", "MCIG", "EC21", "IC21", "LC5", "LC19", "LC20", "LC21"),
    "method" = rep("SSD", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    10
  )
})

test_that("Second highest priorty group (with IC11) selected with ssd method", {
  reps <- 13
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(10, 3, 4, 4, 5, 5, 5, 6, 6, 7, 7, 8, 8),
    "endpoint" = c("IC11", "MATC", "NOEC", "NOEL", "LOEC", "LOEL", "MCIG", "EC21", "IC21", "LC5", "LC19", "LC20", "LC21"),
    "method" = rep("SSD", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    10
  )
})

test_that("Second highest priorty group (with IC15) selected with ssd method", {
  reps <- 13
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(10, 3, 4, 4, 5, 5, 5, 6, 6, 7, 7, 8, 8),
    "endpoint" = c("IC15", "MATC", "NOEC", "NOEL", "LOEC", "LOEL", "MCIG", "EC21", "IC21", "LC5", "LC19", "LC20", "LC21"),
    "method" = rep("SSD", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    10
  )
})

test_that("Second highest priorty group (with IC20) selected with ssd method", {
  reps <- 13
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(10, 3, 4, 4, 5, 5, 5, 6, 6, 7, 7, 8, 8),
    "endpoint" = c("IC20", "MATC", "NOEC", "NOEL", "LOEC", "LOEL", "MCIG", "EC21", "IC21", "LC5", "LC19", "LC20", "LC21"),
    "method" = rep("SSD", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    10
  )
})

test_that("Third highest priorty group (with MATC) selected with ssd method", {
  reps <- 12
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(10, 4, 4, 5, 5, 5, 6, 6, 7, 7, 8, 8),
    "endpoint" = c("MATC", "NOEC", "NOEL", "LOEC", "LOEL", "MCIG", "EC21", "IC21", "LC5", "LC19", "LC20", "LC21"),
    "method" = rep("SSD", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    10
  )
})

test_that("Fourth highest priorty group (with NOEC) selected with ssd method", {
  reps <- 10
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(10, 5, 5, 5, 6, 6, 7, 7, 8, 8),
    "endpoint" = c("NOEC", "LOEC", "LOEL", "MCIG", "EC21", "IC21", "LC5", "LC19", "LC20", "LC21"),
    "method" = rep("SSD", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    10
  )
})

test_that("Fourth highest priorty group (with NOEL) selected with ssd method", {
  reps <- 10
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(10, 5, 5, 5, 6, 6, 7, 7, 8, 8),
    "endpoint" = c("NOEL", "LOEC", "LOEL", "MCIG", "EC21", "IC21", "LC5", "LC19", "LC20", "LC21"),
    "method" = rep("SSD", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    10
  )
})

test_that("Fifth highest priorty group (with LOEC) selected with ssd method", {
  reps <- 7
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(10, 6, 6, 7, 7, 8, 8),
    "endpoint" = c("LOEC", "EC21", "IC21", "LC5", "LC19", "LC20", "LC21"),
    "method" = rep("SSD", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    10
  )
})

test_that("Fifth highest priorty group (with LOEL) selected with ssd method, move to confirm position doesn't matter", {
  reps <- 7
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(6, 6, 10, 7, 7, 8, 8),
    "endpoint" = c("EC21", "IC21", "LOEL",  "LC5", "LC19", "LC20", "LC21"),
    "method" = rep("SSD", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    10
  )
})

test_that("Fifth highest priorty group (with MCIG) selected with ssd method moved to confirm position doesn't matter", {
  reps <- 7
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(6, 6, 7, 10, 7, 8, 8),
    "endpoint" = c("EC21", "IC21", "LC5", "MCIG", "LC19", "LC20", "LC21"),
    "method" = rep("SSD", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    10
  )
})

test_that("Sixth highest priorty group (with EC21) selected with ssd method", {
  reps <- 5
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(10, 7, 7, 8, 8),
    "endpoint" = c("EC21", "LC5", "LC19", "LC20", "LC21"),
    "method" = rep("SSD", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    10
  )
})

test_that("Sixth highest priorty group (with IC21) selected with ssd method", {
  reps <- 5
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(10, 7, 7, 8, 8),
    "endpoint" = c("IC21", "LC5", "LC19", "LC20", "LC21"),
    "method" = rep("SSD", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    10
  )
})

test_that("Seventh highest priorty group (with LC19) selected with ssd method", {
  reps <- 3
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(10, 8, 8),
    "endpoint" = c("LC19", "LC20", "LC21"),
    "method" = rep("SSD", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    10
  )
})

test_that("Seventh highest priorty group (with LC08) selected with ssd method", {
  reps <- 3
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(10, 8, 8),
    "endpoint" = c("LC08", "LC20", "LC21"),
    "method" = rep("SSD", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    10
  )
})

test_that("Second priorty value (EC11) is picked regardless of concentration value and is highest value with ssd method", {
  reps <- 3
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(10, 8, 1),
    "endpoint" = c("EC11", "MATC", "LC08"),
    "method" = rep("SSD", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    10
  )
})

test_that("Second priorty value (EC11) is picked regardless of concentration value when its the medium of the values with ssd method", {
  reps <- 3
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(5, 8, 1),
    "endpoint" = c("EC11", "MATC", "LC08"),
    "method" = rep("SSD", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    5
  )
})

test_that("Second priorty value (EC11) is picked regardless of concentration value when its the lowest of the values with ssd method", {
  reps <- 3
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(0.00001, 8, 1),
    "endpoint" = c("EC11", "MATC", "LC08"),
    "method" = rep("SSD", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    0.00001
  )
})

test_that("Choose highest priority group and calc geometric mean with 1 group", {
  reps <- 4
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(5, 10, 0.4, 1),
    "endpoint" = c("EC05", "EC05", "EC05", "MATC"),
    "method" = rep("SSD", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    round(output$sp_aggre_conc_mg.L, 3),
    2.714
  )
})

test_that("Choose highest priority group and calc geometric mean with 2 groups", {
  reps <- 7
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = rep("Egg", reps),
    "effect" = c(rep("Mortality", 3), rep("Growth", 4)),
    "effect_conc_std_mg.L" = c(5, 10, 0.4, 4, 9, 0.3, 0.5),
    "endpoint" = c("EC05", "EC05", "EC05", "EC05", "EC05", "EC05", "MATC"),
    "method" = rep("SSD", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    round(output$sp_aggre_conc_mg.L, 3),
    2.210
  )
})

test_that("Choose highest priority group and calc geometric mean with ssd method", {
  reps <- 7
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = c(rep("Egg", 3), rep("Larva", 4)),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(5, 10, 0.4, 4, 9, 0.3, 0.5),
    "endpoint" = c("EC05", "EC05", "EC05", "EC05", "EC05", "EC05", "MATC"),
    "method" = rep("SSD", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    round(output$sp_aggre_conc_mg.L, 3),
    2.210
  )
})

test_that("Choose different priority for each species with ssd method", {
  reps <- 6
  df <- data.frame(
    "species_number" = c(1L, 1L, 1L, 2L, 2L, 2L),
    "lifestage" = c(rep("Egg", 3), rep("Larva", 3)),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(5, 10, 0.4, 9, 0.3, 0.5),
    "endpoint" = c("MATC", "LC10", "EC30", "EC05", "MATC", "LC50"),
    "method" = rep("SSD", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    c(5, 9)
  )
})

# Deterministic ----
test_that("Choose highest priority (EC05) endpoint for det method", {
  reps <- 17
  df <- data.frame(
    "species_number" = c(rep(1L, reps)),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(14, 2, 2, 2, 2, 3, 4, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8),
    "endpoint" = c("EC05", "EC11", "EC20", "IC11", "IC20", "MATC", "LOEC", "LOEL", "MCIG", "EC21", "IC21", "LC05", "LC19", "LC20", "LC21", "NOEC", "NOEL"),
    "method" = rep("Deterministic", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    14
  )
})

test_that("Choose highest priority (EC10) endpoint for det method", {
  reps <- 17
  df <- data.frame(
    "species_number" = c(rep(1L, reps)),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(14, 2, 2, 2, 2, 3, 4, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8),
    "endpoint" = c("EC10", "EC11", "EC20", "IC11", "IC20", "MATC", "LOEC", "LOEL", "MCIG", "EC21", "IC21", "LC05", "LC19", "LC20", "LC21", "NOEC", "NOEL"),
    "method" = rep("Deterministic", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    14
  )
})

test_that("Choose highest priority (IC05) endpoint for det method", {
  reps <- 17
  df <- data.frame(
    "species_number" = c(rep(1L, reps)),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(14, 2, 2, 2, 2, 3, 4, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8),
    "endpoint" = c("IC05", "EC11", "EC20", "IC11", "IC20", "MATC", "LOEC", "LOEL", "MCIG", "EC21", "IC21", "LC05", "LC19", "LC20", "LC21", "NOEC", "NOEL"),
    "method" = rep("Deterministic", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    14
  )
})

test_that("Choose highest priority (IC10) endpoint for det method", {
  reps <- 17
  df <- data.frame(
    "species_number" = c(rep(1L, reps)),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(14, 2, 2, 2, 2, 3, 4, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8),
    "endpoint" = c("IC10", "EC11", "EC20", "IC11", "IC20", "MATC", "LOEC", "LOEL", "MCIG", "EC21", "IC21", "LC05", "LC19", "LC20", "LC21", "NOEC", "NOEL"),
    "method" = rep("Deterministic", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    14
  )
})


test_that("Choose 2nd priority (EC11) endpoint for det method", {
  reps <- 13
  df <- data.frame(
    "species_number" = c(rep(1L, reps)),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(14, 3, 4, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8),
    "endpoint" = c("EC11", "MATC", "LOEC", "LOEL", "MCIG", "EC21", "IC21", "LC05", "LC19", "LC20", "LC21", "NOEC", "NOEL"),
    "method" = rep("Deterministic", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    14
  )
})

test_that("Choose 2nd priority (EC20) endpoint for det method, move to show order doesn't matter", {
  reps <- 13
  df <- data.frame(
    "species_number" = c(rep(1L, reps)),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(3, 14, 4, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8),
    "endpoint" = c("MATC", "EC20", "LOEC", "LOEL", "MCIG", "EC21", "IC21", "LC05", "LC19", "LC20", "LC21", "NOEC", "NOEL"),
    "method" = rep("Deterministic", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    14
  )
})

test_that("Choose 2nd priority (IC11) endpoint for det method, move to show order doesn't matter", {
  reps <- 13
  df <- data.frame(
    "species_number" = c(rep(1L, reps)),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(3, 14, 4, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8),
    "endpoint" = c("MATC", "IC11", "LOEC", "LOEL", "MCIG", "EC21", "IC21", "LC05", "LC19", "LC20", "LC21", "NOEC", "NOEL"),
    "method" = rep("Deterministic", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    14
  )
})

test_that("Choose 2nd priority (IC20) endpoint for det method, move to show order doesn't matter", {
  reps <- 13
  df <- data.frame(
    "species_number" = c(rep(1L, reps)),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(3, 4, 14, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8),
    "endpoint" = c("MATC", "LOEC", "IC20", "LOEL", "MCIG", "EC21", "IC21", "LC05", "LC19", "LC20", "LC21", "NOEC", "NOEL"),
    "method" = rep("Deterministic", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    14
  )
})

test_that("Choose 3rd priority (MATC) endpoint for det method", {
  reps <- 12
  df <- data.frame(
    "species_number" = c(rep(1L, reps)),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(14, 4, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8),
    "endpoint" = c("MATC", "LOEC", "LOEL", "MCIG", "EC21", "IC21", "LC05", "LC19", "LC20", "LC21", "NOEC", "NOEL"),
    "method" = rep("Deterministic", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    14
  )
})

test_that("Choose 4th priority (LOEC) endpoint for det method", {
  reps <- 9
  df <- data.frame(
    "species_number" = c(rep(1L, reps)),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(14, 5, 5, 6, 6, 7, 7, 8, 8),
    "endpoint" = c("LOEC", "EC21", "IC21", "LC05", "LC19", "LC20", "LC21", "NOEC", "NOEL"),
    "method" = rep("Deterministic", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    14
  )
})

test_that("Choose 4th priority (LOEL) endpoint for det method", {
  reps <- 9
  df <- data.frame(
    "species_number" = c(rep(1L, reps)),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(14, 5, 5, 6, 6, 7, 7, 8, 8),
    "endpoint" = c("LOEL", "EC21", "IC21", "LC05", "LC19", "LC20", "LC21", "NOEC", "NOEL"),
    "method" = rep("Deterministic", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    14
  )
})

test_that("Choose 4th priority (MCIG) endpoint for det method", {
  reps <- 9
  df <- data.frame(
    "species_number" = c(rep(1L, reps)),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(14, 5, 5, 6, 6, 7, 7, 8, 8),
    "endpoint" = c("MCIG", "EC21", "IC21", "LC05", "LC19", "LC20", "LC21", "NOEC", "NOEL"),
    "method" = rep("Deterministic", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    14
  )
})

test_that("Choose 5th priority (EC21) endpoint for det method", {
  reps <- 7
  df <- data.frame(
    "species_number" = c(rep(1L, reps)),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(14, 6, 6, 7, 7, 8, 8),
    "endpoint" = c("EC21", "LC05", "LC19", "LC20", "LC21", "NOEC", "NOEL"),
    "method" = rep("Deterministic", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    14
  )
})


test_that("Choose 5th priority (IC21) endpoint for det method", {
  reps <- 7
  df <- data.frame(
    "species_number" = c(rep(1L, reps)),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(14, 6, 6, 7, 7, 8, 8),
    "endpoint" = c("IC21", "LC05", "LC19", "LC20", "LC21", "NOEC", "NOEL"),
    "method" = rep("Deterministic", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    14
  )
})

test_that("Choose 6th priority (LC19) endpoint for det method", {
  reps <- 5
  df <- data.frame(
    "species_number" = c(rep(1L, reps)),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(14, 7, 7, 8, 8),
    "endpoint" = c("LC19", "LC20", "LC21", "NOEC", "NOEL"),
    "method" = rep("Deterministic", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    14
  )
})


test_that("Choose 6th priority (LC09) endpoint for det method", {
  reps <- 5
  df <- data.frame(
    "species_number" = c(rep(1L, reps)),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(14, 7, 7, 8, 8),
    "endpoint" = c("LC09", "LC20", "LC21", "NOEC", "NOEL"),
    "method" = rep("Deterministic", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    14
  )
})

test_that("Choose 7th priority (LC21) endpoint for det method", {
  reps <- 3
  df <- data.frame(
    "species_number" = c(rep(1L, reps)),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(14, 8, 8),
    "endpoint" = c("LC21", "NOEC", "NOEL"),
    "method" = rep("Deterministic", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    14
  )
})

test_that("Second priorty value (EC11) is picked regardless of concentration value and is highest value det method", {
  reps <- 3
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(10, 8, 1),
    "endpoint" = c("EC11", "MATC", "LC08"),
    "method" = rep("Deterministic", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    10
  )
})

test_that("Second priorty value (EC11) is picked regardless of concentration value when its the medium of the values for det method", {
  reps <- 3
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(5, 8, 1),
    "endpoint" = c("EC11", "MATC", "LC08"),
    "method" = rep("Deterministic", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    5
  )
})

test_that("Second priorty value (EC11) is picked regardless of concentration value when its the lowest of the values for det method", {
  reps <- 3
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(0.00001, 8, 1),
    "endpoint" = c("EC11", "MATC", "LC08"),
    "method" = rep("Deterministic", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    0.00001
  )
})

test_that("Choose highest priority group and calc geometric mean with 1 group for det method", {
  reps <- 4
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(5, 10, 0.4, 1),
    "endpoint" = c("EC05", "EC05", "EC05", "MATC"),
    "method" = rep("Deterministic", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    round(output$sp_aggre_conc_mg.L, 3),
    2.714
  )
})

test_that("Choose highest priority group and calc geometric mean with 2 groups for det method", {
  reps <- 7
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = rep("Egg", reps),
    "effect" = c(rep("Mortality", 3), rep("Growth", 4)),
    "effect_conc_std_mg.L" = c(5, 10, 0.4, 4, 9, 0.3, 0.5),
    "endpoint" = c("EC05", "EC05", "EC05", "EC05", "EC05", "EC05", "MATC"),
    "method" = rep("Deterministic", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    round(output$sp_aggre_conc_mg.L, 3),
    2.210
  )
})

test_that("Choose highest priority group and calc geometric mean for det method", {
  reps <- 7
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = c(rep("Egg", 3), rep("Larva", 4)),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(5, 10, 0.4, 4, 9, 0.3, 0.5),
    "endpoint" = c("EC05", "EC05", "EC05", "EC05", "EC05", "EC05", "MATC"),
    "method" = rep("Deterministic", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    round(output$sp_aggre_conc_mg.L, 3),
    2.210
  )
})

test_that("Choose different priority for each species with det method", {
  reps <- 6
  df <- data.frame(
    "species_number" = c(1L, 1L, 1L, 2L, 2L, 2L),
    "lifestage" = c(rep("Egg", 3), rep("Larva", 3)),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(5, 10, 0.4, 9, 0.3, 0.5),
    "endpoint" = c("MATC", "LC10", "EC30", "EC05", "MATC", "LC50"),
    "method" = rep("SSD", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    c(5, 9)
  )
})

# Check method priority ----

test_that("Ensure det method priority is used", {
  reps <- 2
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(5, 10),
    "endpoint" = c("NOEL", "LOEL"),
    "method" = rep("Deterministic", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    10
  )
})

test_that("Ensure ssd method priority is used", {
  reps <- 2
  df <- data.frame(
    "species_number" = rep(1L, reps),
    "lifestage" = rep("Egg", reps),
    "effect" = rep("Mortality", reps),
    "effect_conc_std_mg.L" = c(5, 10),
    "endpoint" = c("NOEL", "LOEL"),
    "method" = rep("SSD", reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps), 
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps) 
  )
  output <- wqb_aggregate(df)
  expect_equal(
    output$sp_aggre_conc_mg.L,
    5
  )
})
