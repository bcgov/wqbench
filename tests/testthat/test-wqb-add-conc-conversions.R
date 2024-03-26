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

test_that("combine conc std by codes", {
  concentration_std <- data.frame(
    code = c("--", NA, "NR", "AI mg/dm3", "g/dm3", "PSU"),
    conc_conversion_flag = c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE),
    conc_conversion_value_multiplier = c(NA, NA, NA, 1, 1000, NA),
    conc_conversion_unit = c(NA, NA, NA, "mg/L", "mg/L", NA)
  )

  db_concentration_unit_codes <- data.frame(
    code = c("--", NA, "NR", "PSU", "AI mg/dm3", "g/dm3", "%bt"),
    description = c("unspecified", "missing", "not", "", "active ingrent", "", "percent")
  )

  output <- combine_conc_conversions(concentration_std, db_concentration_unit_codes)

  expect_equal(
    output$code,
    c("--", NA, "NR", "PSU", "AI mg/dm3", "g/dm3", "%bt")
  )
  expect_equal(
    output$conc_conversion_flag,
    c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, NA)
  )
  expect_equal(
    output$conc_conversion_value_multiplier,
    c(NA, NA, NA, NA, 1, 1000, NA)
  )
})

test_that("no error if user adds a space to the entered codes", {
  concentration_std <- data.frame(
    code = c("--", NA, "NR ", "AI  mg/dm3", "g/dm3  ", "PSU"),
    conc_conversion_flag = c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE),
    conc_conversion_value_multiplier = c(NA, NA, NA, 1, 1000, NA),
    conc_conversion_unit = c(NA, NA, NA, "mg/L", "mg/L", NA)
  )

  db_concentration_unit_codes <- data.frame(
    code = c("--", NA, "NR", "PSU", "AI mg/dm3", "g/dm3", "%bt"),
    description = c("unspecified", "missing", "not", "", "active ingrent", "", "percent")
  )

  output <- combine_conc_conversions(concentration_std, db_concentration_unit_codes)

  expect_equal(
    output$code,
    c("--", NA, "NR", "PSU", "AI mg/dm3", "g/dm3", "%bt")
  )
  expect_equal(
    output$conc_conversion_flag,
    c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, NA)
  )
  expect_equal(
    output$conc_conversion_value_multiplier,
    c(NA, NA, NA, NA, 1, 1000, NA)
  )
})

test_that("prints out message indicating which codes may be typos", {
  concentration_std <- data.frame(
    code = c("--", NA, "NRX", "AI mg/dm3", "g/dmm3", "PSU"),
    conc_conversion_flag = c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE),
    conc_conversion_value_multiplier = c(NA, NA, NA, 1, 1000, NA),
    conc_conversion_unit = c(NA, NA, NA, "mg/L", "mg/L", NA)
  )

  db_concentration_unit_codes <- data.frame(
    code = c("--", NA, "NR", "PSU", "AI mg/dm3", "g/dm3", "%bt"),
    description = c("unspecified", "missing", "not", "", "active ingrent", "", "percent")
  )

  expect_message(
    expect_message(
      combine_conc_conversions(concentration_std, db_concentration_unit_codes),
      regexp = "Value\\(s) do not match code\\(s) in `concentration_unit_codes` table in ECOTOX database:"
    ),
    regexp = "'NRX', 'g/dmm3'"
  )
})

test_that("empty conc std means no codes coded to keep", {
  concentration_std <- data.frame(
    code = character(),
    conc_conversion_flag = logical(),
    conc_conversion_value_multiplier = double(),
    conc_conversion_unit = character()
  )

  db_concentration_unit_codes <- data.frame(
    code = c("--", NA, "NR", "PSU", "AI mg/dm3", "g/dm3", "%bt"),
    description = c("unspecified", "missing", "not", "", "active ingrent", "", "percent")
  )

  output <- combine_conc_conversions(concentration_std, db_concentration_unit_codes)

  expect_equal(
    output$code,
    c("--", NA, "NR", "PSU", "AI mg/dm3", "g/dm3", "%bt")
  )
  expect_equal(
    output$conc_conversion_flag,
    c(NA, NA, NA, NA, NA, NA, NA)
  )
  expect_equal(
    output$conc_conversion_value_multiplier,
    c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)
  )
})

test_that("all missing conc std means no codes coded to keep", {
  concentration_std <- data.frame(
    code = NA_character_,
    conc_conversion_flag = NA,
    conc_conversion_value_multiplier = NA_real_,
    conc_conversion_unit = NA_character_
  )

  db_concentration_unit_codes <- data.frame(
    code = c("--", NA, "NR", "PSU", "AI mg/dm3", "g/dm3", "%bt"),
    description = c("unspecified", "missing", "not", "", "active ingrent", "", "percent")
  )

  output <- combine_conc_conversions(concentration_std, db_concentration_unit_codes)

  expect_equal(
    output$code,
    c("--", NA, "NR", "PSU", "AI mg/dm3", "g/dm3", "%bt")
  )
  expect_equal(
    output$conc_conversion_flag,
    c(NA, NA, NA, NA, NA, NA, NA)
  )
  expect_equal(
    output$conc_conversion_value_multiplier,
    c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)
  )
})

test_that("read in actual conc conv file and check message", {
  concentration_std_file_path <- system.file(
    "extdata/concentration-conversion.csv",
    package = "wqbench"
  )

  db_concentration_unit_codes <- data.frame(
    code = c("--", NA, "NR", "PSU", "AI mg/dm3", "g/dm3", "%bt"),
    description = c("unspecified", "missing", "not", "", "active ingrent", "", "percent")
  )

  expect_message(
    expect_message(
      read_conc_conversions(concentration_std_file_path, db_concentration_unit_codes)
    ),
    regexp = c("'%', '% INHIB',")
  )
})
