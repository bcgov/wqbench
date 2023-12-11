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

test_that("combine bc species by cas number in db chemical table", {
  bc_wqg <- data.frame(
    Media = c("Water", "Water", "Water", "Water", "Water"),
    Type = c("Long-term chronic", "Long-term chronic", "Long-term chronic", "Long-term chronic", "Long-term chronic"),
    Use = c("Aquatic Life - Freshwater", "Aquatic Life - Freshwater", "Aquatic Life - Freshwater", "Aquatic Life - Freshwater", "Aquatic Life - Freshwater"),
    CAS_number = c("(7429-90-5)", "(116-06-3)", "(120-12-7)", "(NA)", "(56-55-3)")
  )

  db_chemicals <- data.frame(
    cas_number = c("7429905", "784", "120127", "1234"),
    chemical_name = c("H20", "NH3", "NO3", "NaCl")
  )

  output <- combine_bc_wqg(bc_wqg, db_chemicals)

  expect_equal(
    output$cas_number,
    c("7429905", "784", "120127", "1234")
  )
  expect_equal(
    output$present_in_bc_wqg,
    c(TRUE, FALSE, TRUE, FALSE)
  )
})

test_that("only water media are selected", {
  bc_wqg <- data.frame(
    Media = c("Water", "Water", "Sediment", "Tissue"),
    Type = c("Long-term chronic", "Long-term chronic", "Long-term chronic", "Long-term chronic"),
    Use = c("Aquatic Life - Freshwater", "Aquatic Life - Freshwater", "Aquatic Life - Freshwater", "Aquatic Life - Freshwater"),
    CAS_number = c("(7429-90-5)", "(116-06-3)", "(120-12-7)", "(56-55-3)")
  )

  db_chemicals <- data.frame(
    cas_number = c("7429905", "116063", "120127", "56553"),
    chemical_name = c("H20", "NH3", "NO3", "NaCl")
  )

  output <- combine_bc_wqg(bc_wqg, db_chemicals)

  expect_equal(
    output$cas_number,
    c("7429905", "116063", "120127", "56553")
  )
  expect_equal(
    output$present_in_bc_wqg,
    c(TRUE, TRUE, FALSE, FALSE)
  )
})

test_that("only long term chronic is selected", {
  bc_wqg <- data.frame(
    Media = c("Water", "Water", "Water", "Water"),
    Type = c("Long-term chronic", "Short-term acute", "Maximum Acceptable Concentration", "Long-term chronic"),
    Use = c("Aquatic Life - Freshwater", "Aquatic Life - Freshwater", "Aquatic Life - Freshwater", "Aquatic Life - Freshwater"),
    CAS_number = c("(7429-90-5)", "(116-06-3)", "(120-12-7)", "(56-55-3)")
  )

  db_chemicals <- data.frame(
    cas_number = c("7429905", "116063", "120127", "56553"),
    chemical_name = c("H20", "NH3", "NO3", "NaCl")
  )

  output <- combine_bc_wqg(bc_wqg, db_chemicals)

  expect_equal(
    output$cas_number,
    c("7429905", "116063", "120127", "56553")
  )
  expect_equal(
    output$present_in_bc_wqg,
    c(TRUE, FALSE, FALSE, TRUE)
  )
})

test_that("use is aquatic fresh or marine long term chronic is selected", {
  bc_wqg <- data.frame(
    Media = c("Water", "Water", "Water", "Water"),
    Type = c("Long-term chronic", "Long-term chronic", "Long-term chronic", "Long-term chronic"),
    Use = c("Agriculture - Livestock", "Wildlife", "Aquatic Life - Freshwater", "Aquatic Life - Freshwater"),
    CAS_number = c("(7429-90-5)", "(116-06-3)", "(120-12-7)", "(56-55-3)")
  )

  db_chemicals <- data.frame(
    cas_number = c("7429905", "116063", "120127", "56553"),
    chemical_name = c("H20", "NH3", "NO3", "NaCl")
  )

  output <- combine_bc_wqg(bc_wqg, db_chemicals)

  expect_equal(
    output$cas_number,
    c("7429905", "116063", "120127", "56553")
  )
  expect_equal(
    output$present_in_bc_wqg,
    c(FALSE, FALSE, TRUE, TRUE)
  )
})

test_that("spaces in cas do not cause issues", {
  bc_wqg <- data.frame(
    Media = c("Water", "Water", "Water", "Water"),
    Type = c("Long-term chronic", "Long-term chronic", "Long-term chronic", "Long-term chronic"),
    Use = c("Aquatic Life - Freshwater", "Aquatic Life - Freshwater", "Aquatic Life - Freshwater", "Aquatic Life - Freshwater"),
    CAS_number = c(" (7429-90-5)", "( 116-06-3)", "(120- 12-7)", "(56-55-3)  ")
  )

  db_chemicals <- data.frame(
    cas_number = c("7429905", "116063", "120127", "56553"),
    chemical_name = c("H20", "NH3", "NO3", "NaCl")
  )

  output <- combine_bc_wqg(bc_wqg, db_chemicals)

  expect_equal(
    output$cas_number,
    c("7429905", "116063", "120127", "56553")
  )
  expect_equal(
    output$present_in_bc_wqg,
    c(TRUE, TRUE, TRUE, TRUE)
  )
})

test_that("spaces in Media column causes a mismatch due to filter condition", {
  bc_wqg <- data.frame(
    Media = c(" Water", "Water", "Water", "Water"),
    Type = c("Long-term chronic", "Long-term chronic", "Long-term chronic", "Long-term chronic"),
    Use = c("Aquatic Life - Freshwater", "Aquatic Life - Freshwater", "Aquatic Life - Freshwater", "Aquatic Life - Freshwater"),
    CAS_number = c(" (7429-90-5)", "( 116-06-3)", "(120- 12-7)", "(56-55-3)  ")
  )

  db_chemicals <- data.frame(
    cas_number = c("7429905", "116063", "120127", "56553"),
    chemical_name = c("H20", "NH3", "NO3", "NaCl")
  )

  output <- combine_bc_wqg(bc_wqg, db_chemicals)

  expect_equal(
    output$cas_number,
    c("7429905", "116063", "120127", "56553")
  )
  expect_equal(
    output$present_in_bc_wqg,
    c(FALSE, TRUE, TRUE, TRUE)
  )
})


test_that("no bc_wqg means no chemical is tagged as in wqg", {
  bc_wqg <- data.frame(
    Media = character(),
    Type = character(),
    Use = character(),
    CAS_number = character()
  )

  db_chemicals <- data.frame(
    cas_number = c("7429905", "116063", "120127", "56553"),
    chemical_name = c("H20", "NH3", "NO3", "NaCl")
  )

  output <- combine_bc_wqg(bc_wqg, db_chemicals)

  expect_equal(
    output$cas_number,
    c("7429905", "116063", "120127", "56553")
  )
  expect_equal(
    output$present_in_bc_wqg,
    c(FALSE, FALSE, FALSE, FALSE)
  )
})

test_that("all missing data bc_wqg means no chemical is tagged as in wqg", {
  bc_wqg <- data.frame(
    Media = NA_character_,
    Type = NA_character_,
    Use = NA_character_,
    CAS_number = NA_character_
  )

  db_chemicals <- data.frame(
    cas_number = c("7429905", "116063", "120127", "56553"),
    chemical_name = c("H20", "NH3", "NO3", "NaCl")
  )

  output <- combine_bc_wqg(bc_wqg, db_chemicals)

  expect_equal(
    output$cas_number,
    c("7429905", "116063", "120127", "56553")
  )
  expect_equal(
    output$present_in_bc_wqg,
    c(FALSE, FALSE, FALSE, FALSE)
  )
})

test_that("no db chemicals gives an a table with no rows", {
  bc_wqg <- data.frame(
    Media = c("Water", "Water", "Water", "Water"),
    Type = c("Long-term chronic", "Long-term chronic", "Long-term chronic", "Long-term chronic"),
    Use = c("Agriculture - Livestock", "Wildlife", "Aquatic Life - Freshwater", "Aquatic Life - Freshwater"),
    CAS_number = c("(7429-90-5)", "(116-06-3)", "(120-12-7)", "(56-55-3)")
  )

  db_chemicals <- data.frame(
    cas_number = character(),
    chemical_name = character()
  )

  output <- combine_bc_wqg(bc_wqg, db_chemicals)

  expect_equal(
    nrow(output),
    0L
  )
  expect_equal(
    colnames(output),
    c("cas_number", "chemical_name", "present_in_bc_wqg")
  )
})

test_that("read in actual wqg sheet and check things are added ", {
  db_chemicals <- data.frame(
    cas_number = c("7429905", "116063", "120127", "56553", "1"),
    chemical_name = c("H20", "NH3", "NO3", "NaCl", "H")
  )

  output <- read_bc_wqg(db_chemicals)

  expect_equal(
    nrow(output),
    5L
  )

  expect_equal(
    output$cas_number,
    c("7429905", "116063", "120127", "56553", "1")
  )

  expect_equal(
    output$present_in_bc_wqg,
    c(TRUE, TRUE, TRUE, TRUE, FALSE)
  )
})

test_that("test typo in bc wqg file column name should be CAS_number", {
  bc_wqg <- suppressMessages(
    bcdata::bcdc_get_data(
      record = "85d3990a-ec0a-4436-8ebd-150de3ba0747",
      resource = "6f32a85b-a3d9-44c3-9a14-15175eba25b6"
    )   
  )
  expect_true("CAS_ number" %in% colnames(bc_wqg))
})
