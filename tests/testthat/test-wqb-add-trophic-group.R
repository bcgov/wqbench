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
    phylum_division = c(
      "Arthropoda", "Arthropoda", "Arthropoda", "Bacillariophyta",
      "Annelida", "Chordata", "Chordata", "Arthropoda", "Arthropoda"
    ),
    class = c(
      "Branchiopoda", "Maxillopoda", "Maxillopoda", "Bacillariophyceae",
      NA_character_, "Actinopterygii", "Actinopterygii", "Maxillopoda",
      "Malacostraca"
    ),
    order = c(
      NA_character_, "Copepoda", "Calanoida", NA_character_,
      NA_character_, "Salmoniformes", NA_character_, NA_character_,
      NA_character_
    ),
    family = c(
      NA_character_, NA_character_, NA_character_, NA_character_, "Opheliidae",
      NA_character_, NA_character_, NA_character_, NA_character_
    ),
    trophic_group = c(
      "Invertebrate", "Invertebrate", "Invertebrate", "Algae",
      "Invertebrate", "Fish", "Fish", "Invertebrate",
      "Invertebrate"
    ),
    ecological_group = c(
      "Planktonic Invertebrate", "Other", "Other", "Other",
      "Other", "Salmonid", "Other", "Other", "Other"
    )
  )

  db_species <- data.frame(
    common_name = c(
      "Brook trout", "Bluegill", "Water Flea",
      "Acorn Or Rock Barnacle", "Calanoid Copepod",
      "Copepod Subclass", "Sand Shrimp", "Bristleworm",
      "Diatom"
    ),
    phylum_division = c(
      "Chordata", "Chordata", "Arthropoda", "Arthropoda", "Arthropoda",
      "Arthropoda", "Arthropoda", "Annelida", "Bacillariophyta"
    ),
    class = c(
      "Actinopterygii", "Actinopterygii", "Branchiopoda", "Maxillopoda",
      "Maxillopoda", "Maxillopoda", "Malacostraca", NA_character_,
      "Bacillariophyceae"
    ),
    tax_order = c(
      "Salmoniformes", "Perciformes", "Diplostraca", "Sessilia",
      "Calanoida", "Copepoda", "Decapoda", NA_character_,
      "Naviculales"
    ),
    family = c(
      "Salmonidae", "Centrarchidae", "Daphniidae", "Balanidae", "Metridinidae",
      NA_character_, "Gammaridae", "Opheliidae", "Phaeodactylaceae"
    ),
    species_number = 1:9
  )

  output <- combine_trophic_group(trophic_groups, db_species)

  expect_equal(
    output$common_name,
    c(
      "Brook trout", "Bluegill", "Water Flea", "Acorn Or Rock Barnacle",
      "Calanoid Copepod", "Copepod Subclass", "Sand Shrimp", "Bristleworm",
      "Diatom"
    )
  )
  expect_equal(
    output$ecological_group,
    c(
      "Salmonid", "Other", "Planktonic Invertebrate", "Other", "Other", "Other",
      "Other", "Other", "Other"
    )
  )
  expect_equal(
    output$trophic_group,
    c(
      "Fish", "Fish", "Invertebrate", "Invertebrate", "Invertebrate",
      "Invertebrate", "Invertebrate", "Invertebrate", "Algae"
    )
  )
})

test_that("no trophic groups passed all are set as NA", {
  trophic_groups <- data.frame(
    phylum_division = character(),
    class = character(),
    order = character(),
    family = character(),
    trophic_group = character(),
    ecological_group = character()
  )

  db_species <- data.frame(
    common_name = c(
      "Brook trout", "Bluegill", "Water Flea",
      "Acorn Or Rock Barnacle", "Calanoid Copepod",
      "Copepod Subclass", "Sand Shrimp", "Bristleworm",
      "Diatom"
    ),
    phylum_division = c(
      "Chordata", "Chordata", "Arthropoda", "Arthropoda", "Arthropoda",
      "Arthropoda", "Arthropoda", "Annelida", "Bacillariophyta"
    ),
    class = c(
      "Actinopterygii", "Actinopterygii", "Branchiopoda", "Maxillopoda",
      "Maxillopoda", "Maxillopoda", "Malacostraca", NA_character_,
      "Bacillariophyceae"
    ),
    tax_order = c(
      "Salmoniformes", "Perciformes", "Diplostraca", "Sessilia",
      "Calanoida", "Copepoda", "Decapoda", NA_character_,
      "Naviculales"
    ),
    family = c(
      "Salmonidae", "Centrarchidae", "Daphniidae", "Balanidae", "Metridinidae",
      NA_character_, "Gammaridae", "Opheliidae", "Phaeodactylaceae"
    ),
    species_number = 1:9
  )

  output <- combine_trophic_group(trophic_groups, db_species)

  expect_equal(
    output$common_name,
    c(
      "Brook trout", "Bluegill", "Water Flea",
      "Acorn Or Rock Barnacle", "Calanoid Copepod",
      "Copepod Subclass", "Sand Shrimp",
      "Bristleworm", "Diatom"
    )
  )
  expect_equal(
    output$ecological_group,
    c(
      NA_character_, NA_character_, NA_character_, NA_character_, NA_character_,
      NA_character_, NA_character_, NA_character_, NA_character_
    )
  )
  expect_equal(
    output$trophic_group,
    c(
      NA_character_, NA_character_, NA_character_, NA_character_, NA_character_,
      NA_character_, NA_character_, NA_character_, NA_character_
    )
  )
})

test_that("all missing trophic groups passed all are set as NA", {
  trophic_groups <- data.frame(
    phylum_division = NA_character_,
    class = NA_character_,
    order = NA_character_,
    family = NA_character_,
    trophic_group = NA_character_,
    ecological_group = NA_character_
  )

  db_species <- data.frame(
    common_name = c(
      "Brook trout", "Bluegill", "Water Flea",
      "Acorn Or Rock Barnacle", "Calanoid Copepod",
      "Copepod Subclass", "Sand Shrimp", "Bristleworm",
      "Diatom"
    ),
    phylum_division = c(
      "Chordata", "Chordata", "Arthropoda", "Arthropoda", "Arthropoda",
      "Arthropoda", "Arthropoda", "Annelida", "Bacillariophyta"
    ),
    class = c(
      "Actinopterygii", "Actinopterygii", "Branchiopoda", "Maxillopoda",
      "Maxillopoda", "Maxillopoda", "Malacostraca", NA_character_,
      "Bacillariophyceae"
    ),
    tax_order = c(
      "Salmoniformes", "Perciformes", "Diplostraca", "Sessilia",
      "Calanoida", "Copepoda", "Decapoda", NA_character_,
      "Naviculales"
    ),
    family = c(
      "Salmonidae", "Centrarchidae", "Daphniidae", "Balanidae", "Metridinidae",
      NA_character_, "Gammaridae", "Opheliidae", "Phaeodactylaceae"
    ),
    species_number = 1:9
  )

  output <- combine_trophic_group(trophic_groups, db_species)

  expect_equal(
    output$common_name,
    c(
      "Brook trout", "Bluegill", "Water Flea",
      "Acorn Or Rock Barnacle", "Calanoid Copepod",
      "Copepod Subclass", "Sand Shrimp",
      "Bristleworm", "Diatom"
    )
  )
  expect_equal(
    output$ecological_group,
    c(
      NA_character_, NA_character_, NA_character_, NA_character_, NA_character_,
      NA_character_, NA_character_, NA_character_, NA_character_
    )
  )
  expect_equal(
    output$trophic_group,
    c(
      NA_character_, NA_character_, NA_character_, NA_character_, NA_character_,
      NA_character_, NA_character_, NA_character_, NA_character_
    )
  )
})

test_that("group coded as NA if missing in trophic groups but present in db species", {
  trophic_groups <- data.frame(
    phylum_division = c("Arthropoda", "Arthropoda"),
    class = c("Branchiopoda", "Maxillopoda"),
    order = c(NA_character_, "Calanoida"),
    family = c(NA_character_, NA_character_),
    trophic_group = c("Invertebrate", "Invertebrate"),
    ecological_group = c("Planktonic Invertebrate", "Other")
  )

  db_species <- data.frame(
    common_name = c(
      "Water Flea", "Acorn Or Rock Barnacle", "Calanoid Copepod",
      "Sand Shrimp"
    ),
    phylum_division = c("Arthropoda", "Arthropoda", "Arthropoda", "Arthropoda"),
    class = c("Branchiopoda", "Maxillopoda", "Maxillopoda", "Malacostraca"),
    tax_order = c("Diplostraca", "Sessilia", "Calanoida", "Decapoda"),
    family = c("Daphniidae", "Balanidae", "Pseudodiaptomidae", "Crangonidae"),
    species_number = 1:4
  )

  output <- combine_trophic_group(trophic_groups, db_species)

  expect_equal(
    output$common_name,
    c("Water Flea", "Acorn Or Rock Barnacle", "Calanoid Copepod", "Sand Shrimp")
  )
  expect_equal(
    output$ecological_group,
    c("Planktonic Invertebrate", NA_character_, "Other", NA_character_)
  )
  expect_equal(
    output$trophic_group,
    c("Invertebrate", NA_character_, "Invertebrate", NA_character_)
  )
})

test_that("spaces in values don't mess with joins", {
  trophic_groups <- data.frame(
    phylum_division = c("Arthropoda", "Arthropoda", "Chordata", "Chordata", "Arthropoda"),
    class = c("Branchiopoda ", " Maxillopoda", "Actinopterygii", "Actinopterygii", "Malacostraca"),
    order = c(NA_character_, NA_character_, NA_character_, " Salmoniformes ", NA_character_),
    family = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
    trophic_group = c("Invertebrate   ", "Invertebrate", "Fish", "Fish", "Invertebrate"),
    ecological_group = c("Planktonic Invertebrate", "  Other", "Other", "Salmonid", " Other ")
  )

  db_species <- data.frame(
    common_name = c(
      "Brook trout", "Water Flea", "Acorn Or Rock Barnacle", "Sand Shrimp"
    ),
    phylum_division = c(
      "Chordata", "Arthropoda", "Arthropoda", "Arthropoda"
    ),
    class = c(
      "Actinopterygii", "Branchiopoda", "Maxillopoda", "Malacostraca"
    ),
    tax_order = c(
      "Salmoniformes", "Diplostraca", "Copepoda", "Decapoda"
    ),
    family = c(
      "Salmonidae", "Daphniidae", NA_character_, "Gammaridae"
    ),
    species_number = 1:4
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

test_that("phylum doesn't over ride class", {
  trophic_groups <- data.frame(
    phylum_division = c("Chlorophyta", "Chlorophyta"),
    class = c(NA_character_, "Ulvophyceae"),
    order = c(NA_character_, NA_character_),
    family = c(NA_character_),
    trophic_group = c("Algae", "Algae"),
    ecological_group = c("Other-1", "Other-2")
  )

  db_species <- data.frame(
    common_name = c("Green Algae", "Green Algae"),
    phylum_division = c("Chlorophyta", "Chlorophyta"),
    class = c("Ulvophyceae", "Mamiellophyceae"),
    tax_order = c("Cladophorales", "Mamiellales"),
    family = c("Cladophoraceae", "Bathycoccaceae"),
    species_number = 1:2
  )

  output <- combine_trophic_group(trophic_groups, db_species)

  expect_equal(
    output$common_name,
    c("Green Algae", "Green Algae")
  )
  expect_equal(
    output$ecological_group,
    c("Other-2", "Other-1")
  )
  expect_equal(
    output$trophic_group,
    c("Algae", "Algae")
  )
})

test_that("phylum doesn't over ride class", {
  trophic_groups <- data.frame(
    phylum_division = c("Annelida", "Annelida", "Annelida"),
    class = c(NA_character_, NA_character_, "Clitellata"),
    order = c(NA_character_, "Capitellida", NA_character_),
    family = c("Aeolosomatidae", "Arenicolidae", NA_character_),
    trophic_group = c("Invertebrate", "Invertebrate", "Invertebrate"),
    ecological_group = c("Other", "Other-2", "Other-3")
  )

  db_species <- data.frame(
    common_name = c("Earthworm", "Lugworm", "Leech", "Marine Polychaete Worm"),
    phylum_division = c("Annelida", "Annelida", "Annelida", "Annelida"),
    class = c(NA_character_, NA_character_, "Clitellata", NA_character_),
    tax_order = c(NA_character_, "Capitellida", "Rhynchobdellida", "Canalipalpata"),
    family = c("Aeolosomatidae", "Arenicolidae", "Glossiphoniidae", "Sabellariidae"),
    species_number = 1:4
  )

  output <- combine_trophic_group(trophic_groups, db_species)

  expect_equal(
    output$common_name,
    c("Earthworm", "Lugworm", "Leech", "Marine Polychaete Worm")
  )
  expect_equal(
    output$ecological_group,
    c("Other", "Other-2", "Other-3", NA_character_)
  )
  expect_equal(
    output$trophic_group,
    c("Invertebrate", "Invertebrate", "Invertebrate", NA_character_)
  )
})

test_that("empty db table means no rows returned ", {
  trophic_groups <- data.frame(
    phylum_division = c("Arthropoda", "Arthropoda", "Chordata", "Chordata", "Arthropoda"),
    class = c("Branchiopoda ", "Maxillopoda", "Actinopterygii", "Actinopterygii", "Malacostraca"),
    order = c(NA_character_, NA_character_, NA_character_, " Salmoniformes ", NA_character_),
    family = c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_),
    trophic_group = c("Invertebrate   ", "Invertebrate", "Fish", "Fish", "Invertebrate"),
    ecological_group = c("Planktonic Invertebrate", "  Other", "Other", "Salmonid", " Other ")
  )

  db_species <- data.frame(
    phylum_division = character(),
    common_name = character(),
    class = character(),
    tax_order = character(),
    family = character(),
    species_number = integer()
  )

  output <- combine_trophic_group(trophic_groups, db_species)

  expect_equal(
    nrow(output),
    0L
  )
  expect_equal(
    colnames(output),
    c(
      "phylum_division", "common_name", "class", "tax_order", "family",
      "species_number", "ecological_group", "trophic_group"
    )
  )
})

test_that("read in actual trophic group file", {
  trophic_groups_file_path <- system.file(
    "extdata/trophic-group.csv",
    package = "wqbench"
  )
  db_species <- data.frame(
    common_name = c(
      "Brook trout", "Bluegill", "Water Flea",
      "Acorn Or Rock Barnacle", "Calanoid Copepod",
      "Copepod Subclass", "Sand Shrimp", "Bristleworm",
      "Diatom"
    ),
    phylum_division = c(
      "Chordata", "Chordata", "Arthropoda", "Arthropoda", "Arthropoda",
      "Arthropoda", "Arthropoda", "Annelida", "Bacillariophyta"
    ),
    class = c(
      "Actinopterygii", "Actinopterygii", "Branchiopoda", "Maxillopoda",
      "Maxillopoda", "Maxillopoda", "Malacostraca", NA_character_,
      "Bacillariophyceae"
    ),
    tax_order = c(
      "Salmoniformes", "Perciformes", "Diplostraca", "Sessilia",
      "Calanoida", "Copepoda", "Decapoda", NA_character_,
      "Naviculales"
    ),
    family = c(
      "Salmonidae", "Centrarchidae", "Daphniidae", "Balanidae", "Metridinidae",
      NA_character_, "Gammaridae", "Opheliidae", "Phaeodactylaceae"
    ),
    species_number = 1:9
  )
  output <- read_trophic_group(trophic_groups_file_path, db_species)
  expect_equal(
    output$common_name,
    c(
      "Brook trout", "Bluegill", "Water Flea", "Acorn Or Rock Barnacle",
      "Calanoid Copepod", "Copepod Subclass", "Sand Shrimp", "Bristleworm",
      "Diatom"
    )
  )
  expect_equal(
    output$ecological_group,
    c(
      "Salmonid", "Other", "Planktonic Invertebrate", "Other", "Other", "Other",
      "Other", "Other", "Other"
    )
  )
  expect_equal(
    output$trophic_group,
    c(
      "Fish", "Fish", "Invertebrate", "Invertebrate", "Invertebrate",
      "Invertebrate", "Invertebrate", "Invertebrate", "Algae"
    )
  )
})
