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

test_that("errors when ecological group has missing values", {
  reps <- 1
  df <- data.frame(
    "ecological_group" = factor(rep(NA_character_, reps)),
    "trophic_group" = factor(rep("Fish", reps)),
    "species_present_in_bc" = rep(TRUE, reps),
    "af_variation" = rep(NA, reps),
    "af_salmon" =  rep(NA, reps),
    "af_planktonic" =  rep(NA, reps),
    "af_bc_species" =  rep(NA, reps),
    "species_number" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  expect_error(
    wqb_summary_trophic_groups(df),
    regexp = "`data\\$ecological_group` must not have any missing values"
  )
})

test_that("errors when trophic group has missing values", {
  reps <- 1
  df <- data.frame(
    "ecological_group" = factor(rep("Other", reps)),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "species_present_in_bc" = rep(TRUE, reps),
    "af_variation" = rep(NA, reps),
    "af_salmon" =  rep(NA, reps),
    "af_planktonic" =  rep(NA, reps),
    "af_bc_species" =  rep(NA, reps),
    "species_number" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  expect_error(
    wqb_summary_trophic_groups(df),
    regexp = "`data\\$trophic_group` must not have any missing values"
  )
})

test_that("errors when species present in bc has missing values", {
  reps <- 1
  df <- data.frame(
    "ecological_group" = factor(rep("Other", reps)),
    "trophic_group" = factor(rep("Fish", reps)),
    "species_present_in_bc" = rep(NA, reps),
    "af_variation" = rep(NA, reps),
    "af_salmon" =  rep(NA, reps),
    "af_planktonic" =  rep(NA, reps),
    "af_bc_species" =  rep(NA, reps),
    "species_number" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  expect_error(
    wqb_summary_trophic_groups(df),
    regexp = "`data\\$species_present_in_bc` must not have any missing values"
  )
})

test_that("errors when latin name has missing values", {
  reps <- 1
  df <- data.frame(
    "ecological_group" = factor(rep("Other", reps)),
    "trophic_group" = factor(rep("Fish", reps)),
    "species_present_in_bc" = rep(TRUE, reps),
    "latin_name" = rep(NA_character_, reps),
    "af_variation" = rep(NA, reps),
    "af_salmon" =  rep(NA, reps),
    "af_planktonic" =  rep(NA, reps),
    "af_bc_species" =  rep(NA, reps),
    "species_number" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  expect_error(
    wqb_summary_trophic_groups(df),
    regexp = "`data\\$latin_name` must not have any missing values"
  )
})

test_that("outputs column names and structure match", {
  reps <- 1
  df <- data.frame(
    "ecological_group" = factor(
      rep("Other", reps),
      levels = c("Other", "Salmonid", "Planktonic Invertebrates")
    ),
    "trophic_group" = factor(
      rep("Fish", reps),
      levels = c("Fish", "Plant", "Algae", "Invertebrate", "Amphibian")
    ),
    "latin_name" = rep("Oncorhynchus mykiss", reps),
    "species_present_in_bc" = rep(FALSE, reps),
    "af_variation" = rep(NA, reps),
    "af_salmon" =  rep(NA, reps),
    "af_planktonic" =  rep(NA, reps),
    "af_bc_species" =  rep(NA, reps),
    "species_number" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_summary_trophic_groups(df)
  expect_equal(
    colnames(output),
    c("Consideration", "Result")
  )
  expect_equal(
    output$Result,
    c("Fish", "None", "None", "None")
  )
  expect_equal(
    output$Consideration,
    c("Trophic group(s)", "Salmonid(s)", "Planktonic Invertebrate(s)", "B.C. species")
  )
})

test_that("outputs correct names salmonoid and fish", {
  reps <- 2
  df <- data.frame(
    "ecological_group" = factor(
      c(
        rep("Other", 1L),
        rep("Salmonid", 1L)
      ),
      levels = c("Other", "Salmonid", "Planktonic Invertebrates")
    ),
    "trophic_group" = factor(
      rep("Fish", reps),
      levels = c("Fish", "Plant", "Algae", "Invertebrate", "Amphibian")
    ),
    "species_present_in_bc" = rep(TRUE, reps),
    "latin_name" = c("Pimephales promelas", "Oncorhynchus mykiss"),
    "af_variation" = rep(NA, reps),
    "af_salmon" =  rep(NA, reps),
    "af_planktonic" =  rep(NA, reps),
    "af_bc_species" =  rep(NA, reps),
    "species_number" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_summary_trophic_groups(df)
  expect_equal(
    output$Result,
    c("Fish", "Oncorhynchus mykiss", "None", "Oncorhynchus mykiss, Pimephales promelas")
  )
})

test_that("outputs correct values in salmonid and planktonic inverts", {
  reps <- 3
  df <- data.frame(
    "ecological_group" = factor(
      c("Other", "Salmonid", "Planktonic Invertebrate"),
      levels = c("Other", "Salmonid", "Planktonic Invertebrate")
    ),
    "trophic_group" = factor(
      c(
        rep("Fish", 2L),
        rep("Invertebrate", 1L)
      ),
      levels = c("Fish", "Plant", "Algae", "Invertebrate", "Amphibian")
    ),
    "species_present_in_bc" = c(TRUE, TRUE, FALSE),
    "latin_name" = c("Pimephales promelas", "Oncorhynchus mykiss", "Daphnia pulex"),
    "af_variation" = rep(NA, reps),
    "af_salmon" =  rep(NA, reps),
    "af_planktonic" =  rep(NA, reps),
    "af_bc_species" =  rep(NA, reps),
    "species_number" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_summary_trophic_groups(df)
  expect_equal(
    output$Result,
    c("Fish, Invertebrate", "Oncorhynchus mykiss", "Daphnia pulex", "Oncorhynchus mykiss, Pimephales promelas")
  )
})

test_that("add invert to bc species", {
  reps <- 3
  df <- data.frame(
    "ecological_group" = factor(
      c("Other", "Salmonid", "Planktonic Invertebrate"),
      levels = c("Other", "Salmonid", "Planktonic Invertebrate")
    ),
    "trophic_group" = factor(
      c(
        rep("Fish", 2L),
        rep("Invertebrate", 1L)
      ),
      levels = c("Fish", "Plant", "Algae", "Invertebrate", "Amphibian")
    ),
    "species_present_in_bc" = c(TRUE, TRUE, TRUE),
    "latin_name" = c("Pimephales promelas", "Oncorhynchus mykiss", "Daphnia pulex"),
    "af_variation" = rep(NA, reps),
    "af_salmon" =  rep(NA, reps),
    "af_planktonic" =  rep(NA, reps),
    "af_bc_species" =  rep(NA, reps),
    "species_number" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_summary_trophic_groups(df)
  expect_equal(
    output$Result,
    c("Fish, Invertebrate", "Oncorhynchus mykiss", "Daphnia pulex", "Daphnia pulex, Oncorhynchus mykiss, Pimephales promelas")
  )
})

test_that("add algae trophic group", {
  reps <- 4
  df <- data.frame(
    "ecological_group" = factor(
      c("Other", "Salmonid", "Planktonic Invertebrate", "Other"),
      levels = c("Other", "Salmonid", "Planktonic Invertebrate")
    ),
    "trophic_group" = factor(
      c(
        rep("Fish", 2L),
        rep("Invertebrate", 1L),
        rep("Algae", 1L)
      ),
      levels = c("Fish", "Plant", "Algae", "Invertebrate", "Amphibian")
    ),
    "species_present_in_bc" = c(TRUE, TRUE, TRUE, FALSE),
    "latin_name" = c("Pimephales promelas", "Oncorhynchus mykiss", "Daphnia pulex", "Spirogyra crassa"),
    "af_variation" = rep(NA, reps),
    "af_salmon" =  rep(NA, reps),
    "af_planktonic" =  rep(NA, reps),
    "af_bc_species" =  rep(NA, reps),
    "species_number" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_summary_trophic_groups(df)
  expect_equal(
    output$Result,
    c("Fish, Algae, Invertebrate", "Oncorhynchus mykiss", "Daphnia pulex", "Daphnia pulex, Oncorhynchus mykiss, Pimephales promelas")
  )
})

test_that("add plant trophic group", {
  reps <- 5
  df <- data.frame(
    "ecological_group" = factor(
      c("Other", "Salmonid", "Planktonic Invertebrate", "Other", "Other"),
      levels = c("Other", "Salmonid", "Planktonic Invertebrate")
    ),
    "trophic_group" = factor(
      c(
        rep("Fish", 2L),
        rep("Invertebrate", 1L),
        rep("Algae", 1L),
        rep("Plant", 1L)
      ),
      levels = c("Fish", "Plant", "Algae", "Invertebrate", "Amphibian")
    ),
    "species_present_in_bc" = c(TRUE, TRUE, TRUE, FALSE, TRUE),
    "latin_name" = c("Pimephales promelas", "Oncorhynchus mykiss", "Daphnia pulex", 
                     "Spirogyra crassa", "Potamogeton foliosus"),
    "af_variation" = rep(NA, reps),
    "af_salmon" =  rep(NA, reps),
    "af_planktonic" =  rep(NA, reps),
    "af_bc_species" =  rep(NA, reps),
    "species_number" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_summary_trophic_groups(df)
  expect_equal(
    output$Result,
    c(
      "Fish, Plant, Algae, Invertebrate", 
      "Oncorhynchus mykiss", 
      "Daphnia pulex", 
      "Daphnia pulex, Oncorhynchus mykiss, Pimephales promelas, Potamogeton foliosus"
    )
  )
})

test_that("add amphibian trophic group", {
  reps <- 6
  df <- data.frame(
    "ecological_group" = factor(
      c("Other", "Salmonid", "Planktonic Invertebrate", "Other", "Other", "Other"),
      levels = c("Other", "Salmonid", "Planktonic Invertebrate")
    ),
    "trophic_group" = factor(
      c(
        rep("Fish", 2L),
        rep("Invertebrate", 1L),
        rep("Algae", 1L),
        rep("Plant", 1L),
        rep("Amphibian", 1L)
      ),
      levels = c("Fish", "Plant", "Algae", "Invertebrate", "Amphibian")
    ),
    "species_present_in_bc" = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE),
    "latin_name" = c("Pimephales promelas", "Oncorhynchus mykiss", "Daphnia pulex", 
                     "Spirogyra crassa", "Potamogeton foliosus", "Microhyla ornata"),
    "af_variation" = rep(NA, reps),
    "af_salmon" =  rep(NA, reps),
    "af_planktonic" =  rep(NA, reps),
    "af_bc_species" =  rep(NA, reps),
    "species_number" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_summary_trophic_groups(df)
  expect_equal(
    output$Result,
    c(
      "Fish, Plant, Algae, Invertebrate, Amphibian", 
      "Oncorhynchus mykiss", 
      "Daphnia pulex", 
      "Daphnia pulex, Oncorhynchus mykiss, Pimephales promelas, Potamogeton foliosus"
    )
  )
})

test_that("set all species in bc to false", {
  reps <- 6
  df <- data.frame(
    "ecological_group" = factor(
      c("Other", "Salmonid", "Planktonic Invertebrate", "Other", "Other", "Other"),
      levels = c("Other", "Salmonid", "Planktonic Invertebrate")
    ),
    "trophic_group" = factor(
      c(
        rep("Fish", 2L),
        rep("Invertebrate", 1L),
        rep("Algae", 1L),
        rep("Plant", 1L),
        rep("Amphibian", 1L)
      ),
      levels = c("Fish", "Plant", "Algae", "Invertebrate", "Amphibian")
    ),
    "species_present_in_bc" = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    "latin_name" = c("Pimephales promelas", "Oncorhynchus mykiss", "Daphnia pulex", 
                     "Spirogyra crassa", "Potamogeton foliosus", "Microhyla ornata"),
    "af_variation" = rep(NA, reps),
    "af_salmon" =  rep(NA, reps),
    "af_planktonic" =  rep(NA, reps),
    "af_bc_species" =  rep(NA, reps),
    "species_number" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_summary_trophic_groups(df)
  expect_equal(
    output$Result,
    c(
      "Fish, Plant, Algae, Invertebrate, Amphibian", 
      "Oncorhynchus mykiss", 
      "Daphnia pulex", 
      "None"
    )
  )
})

test_that("set ecological groups to be other", {
  reps <- 6
  df <- data.frame(
    "ecological_group" = factor(
      c("Other", "Other", "Other", "Other", "Other", "Other"),
      levels = c("Other", "Salmonid", "Planktonic Invertebrate")
    ),
    "trophic_group" = factor(
      c(
        rep("Fish", 2L),
        rep("Invertebrate", 1L),
        rep("Algae", 1L),
        rep("Plant", 1L),
        rep("Amphibian", 1L)
      ),
      levels = c("Fish", "Plant", "Algae", "Invertebrate", "Amphibian")
    ),
    "species_present_in_bc" = c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
    "latin_name" = c("Pimephales promelas", "Oncorhynchus mykiss", "Daphnia pulex", 
                     "Spirogyra crassa", "Potamogeton foliosus", "Microhyla ornata"),
    "af_variation" = rep(NA, reps),
    "af_salmon" =  rep(NA, reps),
    "af_planktonic" =  rep(NA, reps),
    "af_bc_species" =  rep(NA, reps),
    "species_number" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "sp_aggre_conc_mg.L" = rep(NA, reps),
    "method" = rep(NA, reps)
  )
  output <- wqb_summary_trophic_groups(df)
  expect_equal(
    output$Result,
    c(
      "Fish, Plant, Algae, Invertebrate, Amphibian", 
      "None", 
      "None", 
      "None"
    )
  )
})
