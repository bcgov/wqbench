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
test_that("error if trophic group has missing values", {
  reps <- 2
  df <- data.frame(
    "trophic_group" = factor(c("fish", NA_character_)),
    "duration_class" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "lifestage" = rep(NA, reps),
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
    wqb_benchmark_method(df),
    regexp = "must not have any missing values"
  )
})

# SSD ----
test_that("SSD when more then 3 fish, more then 3 inverts and more then 1 plant", {
  reps <- 7
  df <- data.frame(
    "trophic_group" = factor(
      c(rep("fish", 3),
      rep("invertebrate", 3),
      rep("plant", 1))
    ),
    "duration_class" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "lifestage" = rep(NA, reps),
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
  output <- wqb_benchmark_method(df)
  expect_equal(
    unique(output$method),
    "SSD"
  )
})

test_that("SSD when more then 4 fish, more then 3 inverts and more then 1 plant", {
  reps <- 8
  df <- data.frame(
    "trophic_group" = factor(
      c(rep("fish", 4),
        rep("invertebrate", 3),
        rep("plant", 1))
    ),
    "duration_class" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "lifestage" = rep(NA, reps),
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
  output <- wqb_benchmark_method(df)
  expect_equal(
    unique(output$method),
    "SSD"
  )
})

test_that("SSD when more then 3 fish, more then 3 inverts and 1 algae", {
  reps <- 7
  df <- data.frame(
    "trophic_group" = factor(
      c(rep("fish", 3),
        rep("invertebrate", 3),
        rep("algae", 1))
    ),
    "duration_class" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "lifestage" = rep(NA, reps),
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
  output <- wqb_benchmark_method(df)
  expect_equal(
    unique(output$method),
    "SSD"
  )
})

# Deterministic ----
test_that("Deterministic when 2 fish, 3 inverts, 1 alage. Because need at least 3 fish.", {
  reps <- 6
  df <- data.frame(
    "trophic_group" = factor(
      c(rep("fish", 2),
        rep("invertebrate", 3),
        rep("algae", 1))
    ),
    "duration_class" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "lifestage" = rep(NA, reps),
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
  output <- wqb_benchmark_method(df)
  expect_equal(
    unique(output$method),
    "Deterministic"
  )
})

test_that("Deterministic when 2 fish, 4 inverts, 1 alage. Because need at least 3 fish.", {
  reps <- 7
  df <- data.frame(
    "trophic_group" = factor(
      c(rep("fish", 2),
        rep("invertebrate", 4),
        rep("algae", 1))
    ),
    "duration_class" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "lifestage" = rep(NA, reps),
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
  output <- wqb_benchmark_method(df)
  expect_equal(
    unique(output$method),
    "Deterministic"
  )
})

test_that("Deterministic when 1 fish, 5 inverts, 5 plants, 5 alage. Because need at least 3 fish.", {
  reps <- 16
  df <- data.frame(
    "trophic_group" = factor(
      c(rep("fish", 1),
        rep("invertebrate", 5),
        rep("algae", 5),
        rep("plant", 5)
      )
    ),
    "duration_class" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "lifestage" = rep(NA, reps),
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
  output <- wqb_benchmark_method(df)
  expect_equal(
    unique(output$method),
    "Deterministic"
  )
})

test_that("Deterministic when 3 fish, 2 inverts, 5 plants, 5 alage. Because need at least 3 inverts.", {
  reps <- 15
  df <- data.frame(
    "trophic_group" = factor(
      c(rep("fish", 3),
        rep("invertebrate", 2),
        rep("algae", 5),
        rep("plant", 5)
      )
    ),
    "duration_class" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "lifestage" = rep(NA, reps),
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
  output <- wqb_benchmark_method(df)
  expect_equal(
    unique(output$method),
    "Deterministic"
  )
})

test_that("Deterministic when 10 fish, 10 inverts, 5 amphibians. Because need at least 1 plant or algae.", {
  reps <- 25
  df <- data.frame(
    "trophic_group" = factor(
      c(rep("fish", 10),
        rep("invertebrate", 10),
        rep("amphibian", 5)
      )
    ),
    "duration_class" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "lifestage" = rep(NA, reps),
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
  output <- wqb_benchmark_method(df)
  expect_equal(
    unique(output$method),
    "Deterministic"
  )
})

test_that("Deterministic when 3 fish, 3 inverts. Because need at least 1 plant or algae.", {
  reps <- 6
  df <- data.frame(
    "trophic_group" = factor(
      c(rep("fish", 3),
        rep("invertebrate", 3),
        rep("amphibian", 0),
        rep("plant", 0),
        rep("algae", 0)
      )
    ),
    "duration_class" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "lifestage" = rep(NA, reps),
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
  output <- wqb_benchmark_method(df)
  expect_equal(
    unique(output$method),
    "Deterministic"
  )
})

test_that("Deterministic when 3 fish, 2 plants+algae. Because need more or more fish.", {
  reps <- 5
  df <- data.frame(
    "trophic_group" = factor(
      c(rep("invertebrate", 3),
        rep("plant", 1),
        rep("algae", 1)
      )
    ),
    "duration_class" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "lifestage" = rep(NA, reps),
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
  output <- wqb_benchmark_method(df)
  expect_equal(
    unique(output$method),
    "Deterministic"
  )
})
