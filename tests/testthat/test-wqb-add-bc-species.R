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

test_that("combine bc species joins correct columns and codes present in bc correctly", {
  bc_species <- data.frame(
    latin_name = c("Aaptos simplex", "Abagrotis apposita"),
    species_present_in_bc = c(TRUE, TRUE)
  )

  db_species <- data.frame(
    latin_name = c("Aaptos simplex", "Lagodon rhomboides"),
    species_number = c(1L, 2L),
    common_name = c("Brown golfball sponge", "Pinfish")
  )

  output <- combine_bc_species(bc_species, db_species)

  expect_equal(
    output$latin_name,
    c("Aaptos simplex", "Lagodon rhomboides")
  )
  expect_equal(
    output$species_present_in_bc,
    c(TRUE, FALSE)
  )
})

test_that("removes extra space in input species", {
  bc_species <- data.frame(
    latin_name = c("Aaptos simplex  ", " Lagodon rhomboides"),
    species_present_in_bc = c(TRUE, TRUE)
  )

  db_species <- data.frame(
    latin_name = c("Aaptos simplex", "Lagodon rhomboides"),
    species_number = c(1L, 2L),
    common_name = c("Brown golfball sponge", "Pinfish")
  )

  output <- combine_bc_species(bc_species, db_species)

  expect_equal(
    output$latin_name,
    c("Aaptos simplex", "Lagodon rhomboides")
  )
  expect_equal(
    output$species_present_in_bc,
    c(TRUE, TRUE)
  )
})

test_that("empty bc species passed means no species codes as in bc", {
  bc_species <- data.frame(
    latin_name = character(),
    species_present_in_bc = logical()
  )

  db_species <- data.frame(
    latin_name = c("Aaptos simplex", "Lagodon rhomboides"),
    species_number = c(1L, 2L),
    common_name = c("Brown golfball sponge", "Pinfish")
  )

  output <- combine_bc_species(bc_species, db_species)

  expect_equal(
    output$latin_name,
    c("Aaptos simplex", "Lagodon rhomboides")
  )
  expect_equal(
    output$species_present_in_bc,
    c(FALSE, FALSE)
  )
})

test_that("all missing values in bc species passed means no species codes as in bc", {
  bc_species <- data.frame(
    latin_name = NA_character_,
    species_present_in_bc = NA_character_
  )

  db_species <- data.frame(
    latin_name = c("Aaptos simplex", "Lagodon rhomboides"),
    species_number = c(1L, 2L),
    common_name = c("Brown golfball sponge", "Pinfish")
  )

  output <- combine_bc_species(bc_species, db_species)

  expect_equal(
    output$latin_name,
    c("Aaptos simplex", "Lagodon rhomboides")
  )
  expect_equal(
    output$species_present_in_bc,
    c(FALSE, FALSE)
  )
})

test_that("returns an empty dataframe with the proper column headers", {
  bc_species <- data.frame(
    latin_name = c("Aaptos simplex  ", " Lagodon rhomboides"),
    species_present_in_bc = c(TRUE, TRUE)
  )

  db_species <- data.frame(
    latin_name = character(),
    species_number = integer(),
    common_name = character()
  )

  output <- combine_bc_species(bc_species, db_species)

  expect_equal(
    colnames(output),
    c("latin_name", "species_number", "common_name", "species_present_in_bc")
  )
  expect_equal(
    nrow(output),
    0L
  )
})

test_that("reads in bc species csv and joins to db_species as expected", {
  bc_species_file_path <- system.file(
    "extdata/bc-species.csv",
    package = "wqbench"
  )

  db_species <- data.frame(
    latin_name = c("Aaptos simplex", "Lagodon rhomboides", "Abagrotis duanca"),
    species_number = c(1L, 2L, 3L),
    common_name = c("Brown golfball sponge", "Pinfish", "Moth")
  )

  output <- read_bc_species(bc_species_file_path, db_species)

  expect_equal(
    output$latin_name,
    c("Aaptos simplex", "Lagodon rhomboides", "Abagrotis duanca")
  )
  expect_equal(
    output$species_present_in_bc,
    c(TRUE, FALSE, TRUE)
  )
  expect_equal(
    colnames(output),
    c(colnames(db_species), "species_present_in_bc")
  )
})
