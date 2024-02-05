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

test_that("combine duration conversions by codes", {
  duration_std <- data.frame(
    code = c("brd", "bt", "d", "dapu", "h", "hns"),
    description = c(
      "brood or litter", "boot stage", "days",
      "days after pupation", "hours", "haun stage"
    ),
    duration_units_to_keep = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE),
    duration_value_multiplier_to_hours = c(NA_real_, NA_real_, 24, 24, 1, NA_real_),
    comments = c(NA_character_, NA_character_, "24 hours in a day", 
                 "24 hours in a day", "1 hour in an hour", NA_character_)
  )

  db_duration_unit_codes <- data.frame(
    code = c("--", "-X", "brd", "bt", "d", "dapu", "h", "hns"),
    description = c(
      "unspecified", "pretreatment", "brood or litter", "boot stage",
      "days", "days after pupation", "hours", "haun stage"
    )
  )

  output <- combine_duration_conversions(
    duration_std, db_duration_unit_codes
  )

  expect_equal(
    output$code,
    c("--", "-X", "brd", "bt", "d", "dapu", "h", "hns")
  )
  expect_equal(
    output$duration_units_to_keep,
    c(NA, NA, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE)
  )
  expect_equal(
    output$duration_value_multiplier_to_hours,
    c(NA_real_, NA_real_, NA_real_, NA_real_, 24, 24, 1, NA_real_)
  )
})

test_that("spaces in duration conversion codes doesn't cause issues", {
  duration_std <- data.frame(
    code = c(" brd", "bt  ", "d", "dapu     ", "h", " hns "),
    description = c(
      "brood or litter", "boot stage", "days",
      "days after pupation", "hours", "haun stage"
    ),
    duration_units_to_keep = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE),
    duration_value_multiplier_to_hours = c(NA_real_, NA_real_, 24, 24, 1, NA_real_),
    comments = c(NA_character_, NA_character_, "24 hours in a day", 
                 "24 hours in a day", "1 hour in an hour", NA_character_)
  )

  db_duration_unit_codes <- data.frame(
    code = c("--", "-X", "brd", "bt", "d", "dapu", "h", "hns"),
    description = c(
      "unspecified", "pretreatment", "brood or litter", "boot stage",
      "days", "days after pupation", "hours", "haun stage"
    )
  )

  output <- combine_duration_conversions(
    duration_std, db_duration_unit_codes
  )

  expect_equal(
    output$code,
    c("--", "-X", "brd", "bt", "d", "dapu", "h", "hns")
  )
  expect_equal(
    output$duration_units_to_keep,
    c(NA, NA, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE)
  )
  expect_equal(
    output$duration_value_multiplier_to_hours,
    c(NA_real_, NA_real_, NA_real_, NA_real_, 24, 24, 1, NA_real_)
  )
})

test_that("-X excel error adjustment", {
  duration_std <- data.frame(
    code = c("#NAME?", "bt", "d", "dapu"),
    description = c(
      "Pretreatment, time unknown", "boot stage", "days",
      "days after pupation"
    ),
    duration_units_to_keep = c(FALSE, FALSE, TRUE, TRUE),
    duration_value_multiplier_to_hours = c(NA_real_, NA_real_, 24, 24),
    comments = c(NA_character_, NA_character_,
                 "24 hours in a day", "24 hours in a day")
  )

  db_duration_unit_codes <- data.frame(
    code = c("--", "-X", "bt", "d", "dapu"),
    description = c(
      "unspecified", "pretreatment", "boot stage",
      "days", "days after pupation"
    )
  )

  output <- combine_duration_conversions(
    duration_std, db_duration_unit_codes
  )

  expect_equal(
    output$code,
    c("--", "-X", "bt", "d", "dapu")
  )
  expect_equal(
    output$duration_units_to_keep,
    c(NA, FALSE, FALSE, TRUE, TRUE)
  )
  expect_equal(
    output$duration_value_multiplier_to_hours,
    c(NA_real_, NA_real_, NA_real_, 24, 24)
  )
})

test_that("empty duration conversion codes means all coded as NA", {
  duration_std <- data.frame(
    code = character(),
    description = character(),
    duration_units_to_keep = logical(),
    duration_value_multiplier_to_hours = double(),
    comments = character()
  )

  db_duration_unit_codes <- data.frame(
    code = c("--", "-X", "brd", "bt", "d", "dapu", "h", "hns"),
    description = c(
      "unspecified", "pretreatment", "brood or litter", "boot stage",
      "days", "days after pupation", "hours", "haun stage"
    )
  )

  output <- combine_duration_conversions(
    duration_std, db_duration_unit_codes
  )

  expect_equal(
    output$code,
    c("--", "-X", "brd", "bt", "d", "dapu", "h", "hns")
  )
  expect_equal(
    output$duration_units_to_keep,
    c(NA, NA, NA, NA, NA, NA, NA, NA)
  )
  expect_equal(
    output$duration_value_multiplier_to_hours,
    c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)
  )
})

test_that("missing duration conversion codes means all coded as NA", {
  duration_std <- data.frame(
    code = NA_character_,
    description = NA_character_,
    duration_units_to_keep = NA,
    duration_value_multiplier_to_hours = NA_real_,
    comments = NA_character_
  )

  db_duration_unit_codes <- data.frame(
    code = c("--", "-X", "brd", "bt", "d", "dapu", "h", "hns"),
    description = c(
      "unspecified", "pretreatment", "brood or litter", "boot stage",
      "days", "days after pupation", "hours", "haun stage"
    )
  )

  output <- suppressMessages(combine_duration_conversions(
    duration_std, db_duration_unit_codes
  ))

  expect_equal(
    output$code,
    c("--", "-X", "brd", "bt", "d", "dapu", "h", "hns")
  )
  expect_equal(
    output$duration_units_to_keep,
    c(NA, NA, NA, NA, NA, NA, NA, NA)
  )
  expect_equal(
    output$duration_value_multiplier_to_hours,
    c(NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_)
  )
})

test_that("empty db table means no rows are returned", {
  duration_std <- data.frame(
    code = c("brd", "bt", "d", "dapu", "h", "hns"),
    description = c(
      "brood or litter", "boot stage", "days",
      "days after pupation", "hours", "haun stage"
    ),
    duration_units_to_keep = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE),
    duration_value_multiplier_to_hours = c(NA_real_, NA_real_, 24, 24, 1, NA_real_),
    comments = c(NA_character_, NA_character_, "24 hours in a day", 
                 "24 hours in a day", "1 hour in an hour", NA_character_)
  )

  db_duration_unit_codes <- data.frame(
    code = character(),
    description = character()
  )

  output <- suppressMessages(combine_duration_conversions(
    duration_std, db_duration_unit_codes
  ))

  expect_equal(
    nrow(output),
    0L
  )
  expect_equal(
    colnames(output),
    c(
      "code", "description", "duration_units_to_keep",
      "duration_value_multiplier_to_hours", "comments"
    )
  )
})

test_that("message when values are not present in the db table", {
  duration_std <- data.frame(
    code = c("brd", "btt", "d(s)", "dapu", "h", "hns"),
    description = c(
      "brood or litter", "boot stage", "days",
      "days after pupation", "hours", "haun stage"
    ),
    duration_units_to_keep = c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE),
    duration_value_multiplier_to_hours = c(NA_real_, NA_real_, 24, 24, 1, NA_real_),
    comments = c(NA_character_, NA_character_, "24 hours in a day", 
                 "24 hours in a day", "1 hour in an hour", NA_character_)
  )

  db_duration_unit_codes <- data.frame(
    code = c("brd", "bt", "d", "dapu", "h", "hns"),
    description = c(
      "brood or litter", "boot stage", "days",
      "days after pupation", "hours", "haun stage"
    )
  )

  expect_message(
    expect_message(
      combine_duration_conversions(
        duration_std, db_duration_unit_codes
      ),
      regexp = "Value\\(s\\) do not match code\\(s\\) in `duration_unit_codes` table in ECOTOX database:"
    ),
    regexp = "'btt', 'd\\(s\\)'"
  )
})

test_that("read in actual duration conversion file", {
  duration_std_file_path <- system.file(
    "extdata/duration-conversion.csv",
    package = "wqbench"
  )

  db_duration_unit_codes <- data.frame(
    code = c("brd", "bt", "d", "dapu", "h", "hns", "mpf"),
    description = c(
      "brood or litter", "boot stage", "days",
      "days after pupation", "hours", "haun stage",
      "minutes post fert"
    )
  )

  output <- suppressMessages(
    read_duration_conversions(duration_std_file_path, db_duration_unit_codes)
  )

  expect_equal(
    output$code,
    c("brd", "bt", "d", "dapu", "h", "hns", "mpf")
  )
  expect_equal(
    output$duration_units_to_keep,
    c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE)
  )
  expect_equal(
    output$duration_value_multiplier_to_hours,
    c(NA_real_, NA_real_, 24, 24, 1, NA_real_, 0.016667),
    tolerance = 0.0001
  )
})

test_that("read in actual duration conversion file and check message", {
  duration_std_file_path <- system.file(
    "extdata/duration-conversion.csv",
    package = "wqbench"
  )

  db_duration_unit_codes <- data.frame(
    code = c("brd", "bt", "d", "dapu", "h", "hns", "mpf"),
    description = c(
      "brood or litter", "boot stage", "days",
      "days after pupation", "hours", "haun stage", "minutes post fert"
    )
  )

  expect_message(
    expect_message(
      read_duration_conversions(duration_std_file_path, db_duration_unit_codes),
    ),
    regexp = "'--', '-X', 'abs', 'ac', 'alv', 'ant', 'b0.25', 'bbch', \\.\\.\\., 'NA'"
  )
})
