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

test_that("combine species by class and order", {
  trophic_groups <- data.frame(
    class = c("Branchiopoda", "Maxillopoda", "Maxillopoda", "Bacillariophyceae", 
              NA_character_, "Actinopterygii",  "Actinopterygii", "Maxillopoda",
              "Malacostraca"),
    order = c(NA_character_, "Copepoda", "Calanoida", NA_character_, 
              "Canalipalpata", "Salmoniformes", NA_character_, NA_character_,
              NA_character_),
    trophic_group = c("Invertebrate", "Invertebrate", "Invertebrate", "Algae", 
                      "Invertebrate", "Fish", "Fish", "Invertebrate", 
                      "Invertebrate"),
    ecological_group = c("Planktonic Invertebrate", "Other", "Other", "Other", 
                         "Other", "Salmonid", "Other", "Other", "Other")
  )
  
  db_species <- data.frame(
    common_name = c("Brook trout", "Bluegill", "Water Flea", 
                    "Acorn Or Rock Barnacle", "Calanoid Copepod", 
                    "Copepod Subclass", "Sand Shrimp"),
    class = c("Actinopterygii", "Actinopterygii", "Branchiopoda", "Maxillopoda", 
              "Maxillopoda", "Maxillopoda", "Malacostraca"),
    tax_order = c("Salmoniformes", "Perciformes", "Diplostraca", "Sessilia",
                  "Calanoida", "Copepoda", "Decapoda")
  ) 
  
  output <- combine_trophic_group(trophic_groups, db_species)
  
  expect_equal(
    output$common_name,
    c("Brook trout", "Bluegill", "Water Flea", 
      "Acorn Or Rock Barnacle", "Calanoid Copepod", 
      "Copepod Subclass", "Sand Shrimp")
  )
  expect_equal(
    output$ecological_group,
    c("Salmonid", "Other", "Planktonic Invertebrate", "Other", "Other", "Other",
      "Other")
  )
  expect_equal(
    output$trophic_group,
    c("Fish", "Fish", "Invertebrate", "Invertebrate", "Invertebrate", 
      "Invertebrate", "Invertebrate")
  )
})

test_that("no trophic groups passed all are set as NA", {
  trophic_groups <- data.frame(
    class = character(),
    order = character(),
    trophic_group = character(),
    ecological_group = character()
  )
  
  db_species <- data.frame(
    common_name = c("Brook trout", "Bluegill", "Water Flea", 
                    "Acorn Or Rock Barnacle", "Calanoid Copepod", 
                    "Copepod Subclass", "Sand Shrimp"),
    class = c("Actinopterygii", "Actinopterygii", "Branchiopoda", "Maxillopoda", 
              "Maxillopoda", "Maxillopoda", "Malacostraca"),
    tax_order = c("Salmoniformes", "Perciformes", "Diplostraca", "Sessilia",
                  "Calanoida", "Copepoda", "Decapoda")
  ) 
  
  output <- combine_trophic_group(trophic_groups, db_species)
  
  expect_equal(
    output$common_name,
    c("Brook trout", "Bluegill", "Water Flea", 
      "Acorn Or Rock Barnacle", "Calanoid Copepod", 
      "Copepod Subclass", "Sand Shrimp")
  )
  expect_equal(
    output$ecological_group,
    c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, 
      NA_character_, NA_character_)
  )
  expect_equal(
    output$trophic_group,
    c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, 
      NA_character_, NA_character_)
  )
})

test_that("all missing trophic groups passed all are set as NA", {
  trophic_groups <- data.frame(
    class = NA_character_,
    order = NA_character_,
    trophic_group = NA_character_,
    ecological_group = NA_character_
  )
  
  db_species <- data.frame(
    common_name = c("Brook trout", "Bluegill", "Water Flea", 
                    "Acorn Or Rock Barnacle", "Calanoid Copepod", 
                    "Copepod Subclass", "Sand Shrimp"),
    class = c("Actinopterygii", "Actinopterygii", "Branchiopoda", "Maxillopoda", 
              "Maxillopoda", "Maxillopoda", "Malacostraca"),
    tax_order = c("Salmoniformes", "Perciformes", "Diplostraca", "Sessilia",
                  "Calanoida", "Copepoda", "Decapoda")
  ) 
  
  output <- combine_trophic_group(trophic_groups, db_species)
  
  expect_equal(
    output$common_name,
    c("Brook trout", "Bluegill", "Water Flea", 
      "Acorn Or Rock Barnacle", "Calanoid Copepod", 
      "Copepod Subclass", "Sand Shrimp")
  )
  expect_equal(
    output$ecological_group,
    c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, 
      NA_character_, NA_character_)
  )
  expect_equal(
    output$trophic_group,
    c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_, 
      NA_character_, NA_character_)
  )
})

test_that("group coded as NA if missing in trophic groups but present in db species", {
  trophic_groups <- data.frame(
    class = c("Branchiopoda", "Maxillopoda"),
    order = c(NA_character_, "Copepoda"),
    trophic_group = c("Invertebrate", "Invertebrate"),
    ecological_group = c("Planktonic Invertebrate", "Other")
  )
  
  db_species <- data.frame(
    common_name = c("Water Flea", "Acorn Or Rock Barnacle", "Calanoid Copepod", 
                    "Sand Shrimp"),
    class = c("Branchiopoda", "Maxillopoda", "Maxillopoda", "Malacostraca"),
    tax_order = c("Diplostraca", "Sessilia", "Calanoida", "Decapoda")
  ) 
  
  output <- combine_trophic_group(trophic_groups, db_species)
  
  expect_equal(
    output$common_name,
    c("Water Flea", "Acorn Or Rock Barnacle", "Calanoid Copepod", "Sand Shrimp")
  )
  expect_equal(
    output$ecological_group,
    c("Planktonic Invertebrate", NA_character_, NA_character_, NA_character_)
  )
  expect_equal(
    output$trophic_group,
    c("Invertebrate", NA_character_, NA_character_, NA_character_)
  )
})

test_that("spaces in values don't mess with joins", {
  trophic_groups <- data.frame(
    class = c("Branchiopoda ", " Maxillopoda", "Actinopterygii",  "Actinopterygii", "Malacostraca"),
    order = c(NA_character_, NA_character_, NA_character_, " Salmoniformes ", NA_character_),
    trophic_group = c("Invertebrate   ", "Invertebrate", "Fish", "Fish", "Invertebrate"),
    ecological_group = c("Planktonic Invertebrate", "  Other", "Other", "Salmonid", " Other ")
  )
  
  db_species <- data.frame(
    common_name = c("Brook trout", "Water Flea", "Acorn Or Rock Barnacle", 
                    "Sand Shrimp"),
    class = c("Actinopterygii","Branchiopoda", "Maxillopoda", "Malacostraca"),
    tax_order = c("Salmoniformes", "Diplostraca", "Sessilia", "Decapoda")
  ) 
  
  output <- combine_trophic_group(trophic_groups, db_species)
  
  expect_equal(
    output$common_name,
    c("Brook trout", "Water Flea", "Acorn Or Rock Barnacle", "Sand Shrimp")
  )
  expect_equal(
    output$ecological_group,
    c("Salmonid", "Planktonic Invertebrate", "Other", "Other")
  )
  expect_equal(
    output$trophic_group,
    c("Fish", "Invertebrate", "Invertebrate", "Invertebrate")
  )
})

test_that("empty db table means no rows returned ", {
  trophic_groups <- data.frame(
    class = c("Branchiopoda ", " Maxillopoda", "Actinopterygii",  "Actinopterygii", "Malacostraca"),
    order = c(NA_character_, NA_character_, NA_character_, " Salmoniformes ", NA_character_),
    trophic_group = c("Invertebrate   ", "Invertebrate", "Fish", "Fish", "Invertebrate"),
    ecological_group = c("Planktonic Invertebrate", "  Other", "Other", "Salmonid", " Other ")
  )
  
  db_species <- data.frame(
    common_name = character(),
    class = character(),
    tax_order = character()
  ) 
  
  output <- combine_trophic_group(trophic_groups, db_species)
  
  expect_equal(
    nrow(output),
    0L
  )
  expect_equal(
    colnames(output),
    c("common_name", "class", "tax_order", "ecological_group", "trophic_group")
  )
})

test_that("read in actual trophic group file", {
  trophic_groups_file_path <- system.file(
    "extdata/trophic-group.csv",
    package = "wqbench"
  )
  db_species <- data.frame(
    common_name = c("Brook trout", "Bluegill", "Water Flea", 
                    "Acorn Or Rock Barnacle", "Calanoid Copepod", 
                    "Copepod Subclass", "Sand Shrimp", "Horsehair Worm"),
    class = c("Actinopterygii", "Actinopterygii", "Branchiopoda", "Maxillopoda", 
              "Maxillopoda", "Maxillopoda", "Malacostraca", NA_character_),
    tax_order = c("Salmoniformes", "Perciformes", "Diplostraca", "Sessilia",
                  "Calanoida", "Copepoda", "Decapoda", "Gordioidea")
  ) 
  output <- read_trophic_group(trophic_groups_file_path, db_species)
  expect_equal(
    output$common_name,
    c("Brook trout", "Bluegill", "Water Flea", "Acorn Or Rock Barnacle", 
      "Calanoid Copepod", "Copepod Subclass", "Sand Shrimp", "Horsehair Worm")
  )
  expect_equal(
    output$ecological_group,
    c("Salmonid", "Other", "Planktonic Invertebrate", "Other", "Other", "Other",
      "Other", "Other")
  )
  expect_equal(
    output$trophic_group,
    c("Fish", "Fish", "Invertebrate", "Invertebrate", "Invertebrate",
      "Invertebrate", "Invertebrate", "Invertebrate")
  )
})
