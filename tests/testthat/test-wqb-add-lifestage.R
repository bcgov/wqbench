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

test_that("combine life stage by codes", {
  lifestage_codes <- data.frame(
    code = c("--", "AD", "EG", "YY", "ZY"),
    simple_lifestage = c("Adult", "Adult", "ELS", "Juvenile", "ELS")
  )

  db_lifestage_codes <- data.frame(
    code = c("--", "AD", "BD", "EG", "BT", "YY", "ZY"),
    description = c("unspecified", "adult", "bud", "egg", "boot", "young", "zygote")
  )

  output <- combine_lifestage(
    lifestage_codes, db_lifestage_codes
  )

  expect_equal(
    output$code,
    c("--", "AD", "BD", "EG", "BT", "YY", "ZY")
  )
  expect_equal(
    output$simple_lifestage,
    c("adult", "adult", NA_character_, "els", NA_character_, "juvenile", "els")
  )
})

test_that("space in lifestage codes doesn't cause issue", {
  lifestage_codes <- data.frame(
    code = c("--", " AD", "EG    ", " YY ", "ZY"),
    simple_lifestage = c("Adult", "Adult", "ELS", "Juvenile", "ELS")
  )

  db_lifestage_codes <- data.frame(
    code = c("--", "AD", "BD", "EG", "BT", "YY", "ZY"),
    description = c("unspecified", "adult", "bud", "egg", "boot", "young", "zygote")
  )

  output <- combine_lifestage(
    lifestage_codes, db_lifestage_codes
  )

  expect_equal(
    output$code,
    c("--", "AD", "BD", "EG", "BT", "YY", "ZY")
  )
  expect_equal(
    output$simple_lifestage,
    c("adult", "adult", NA_character_, "els", NA_character_, "juvenile", "els")
  )
})

test_that("space in simple lifestage doesn't cause issue", {
  lifestage_codes <- data.frame(
    code = c("--", "AD", "EG", "YY", "ZY"),
    simple_lifestage = c("Adult    ", " Adult", " ELS ", "Juvenile", "ELS")
  )

  db_lifestage_codes <- data.frame(
    code = c("--", "AD", "BD", "EG", "BT", "YY", "ZY"),
    description = c("unspecified", "adult", "bud", "egg", "boot", "young", "zygote")
  )

  output <- combine_lifestage(
    lifestage_codes, db_lifestage_codes
  )

  expect_equal(
    output$code,
    c("--", "AD", "BD", "EG", "BT", "YY", "ZY")
  )
  expect_equal(
    output$simple_lifestage,
    c("adult", "adult", NA_character_, "els", NA_character_, "juvenile", "els")
  )
})

test_that("empty lifestage codes means all simple lifestage are NA", {
  lifestage_codes <- data.frame(
    code = character(),
    simple_lifestage = character()
  )

  db_lifestage_codes <- data.frame(
    code = c("--", "AD", "BD", "EG", "BT", "YY", "ZY"),
    description = c("unspecified", "adult", "bud", "egg", "boot", "young", "zygote")
  )

  output <- combine_lifestage(
    lifestage_codes, db_lifestage_codes
  )

  expect_equal(
    output$code,
    c("--", "AD", "BD", "EG", "BT", "YY", "ZY")
  )
  expect_equal(
    output$simple_lifestage,
    c(
      NA_character_, NA_character_, NA_character_, NA_character_,
      NA_character_, NA_character_, NA_character_
    )
  )
})

test_that("missing lifestage codes means all simple lifestage are NA", {
  lifestage_codes <- data.frame(
    code = NA_character_,
    simple_lifestage = NA_character_
  )

  db_lifestage_codes <- data.frame(
    code = c("--", "AD", "BD", "EG", "BT", "YY", "ZY"),
    description = c("unspecified", "adult", "bud", "egg", "boot", "young", "zygote")
  )

  output <- suppressMessages(
    combine_lifestage(lifestage_codes, db_lifestage_codes)
  )

  expect_equal(
    output$code,
    c("--", "AD", "BD", "EG", "BT", "YY", "ZY")
  )
  expect_equal(
    output$simple_lifestage,
    c(
      NA_character_, NA_character_, NA_character_, NA_character_,
      NA_character_, NA_character_, NA_character_
    )
  )
})

test_that("empty db table means no rows are returned", {
  lifestage_codes <- data.frame(
    code = c("--", " AD", "EG    ", " YY ", "ZY"),
    simple_lifestage = c("Adult", "Adult", "ELS", "Juvenile", "ELS")
  )

  db_lifestage_codes <- data.frame(
    code = character(),
    description = character()
  )

  output <- suppressMessages(
    combine_lifestage(lifestage_codes, db_lifestage_codes)
  )
  expect_equal(
    nrow(output),
    0L
  )
  expect_equal(
    colnames(output),
    c("code", "description", "simple_lifestage")
  )
})

test_that("message when codes are not in db table", {
  lifestage_codes <- data.frame(
    code = c("--", " AD", "EG    ", " YY ", "ZY"),
    simple_lifestage = c("Adult", "Adult", "ELS", "Juvenile", "ELS")
  )

  db_lifestage_codes <- data.frame(
    code = c("--", "AD", "BD", "EG", "BT", "YY"),
    description = c("unspecified", "adult", "bud", "egg", "boot", "young")
  )

  expect_message(
    expect_message(
      combine_lifestage(lifestage_codes, db_lifestage_codes),
      regexp = "Value\\(s\\) do not match code\\(s\\) in `endpoint_code` table in ECOTOX database:"
    ),
    regexp = "'ZY'"
  )
})

test_that("read in actual lifestage codes file", {
  lifestage_file_path <- system.file(
    "extdata/lifestage-codes.csv",
    package = "wqbench"
  )

  db_lifestage_codes <- data.frame(
    code = c("--", "AD", "BD", "EG", "BT", "YY", "F2"),
    description = c("unspecified", "adult", "bud", "egg", "boot", "young", "F1 gen")
  )

  output <- suppressMessages(
    read_lifestage(lifestage_file_path, db_lifestage_codes)
  )

  expect_equal(
    output$code,
    c("--", "AD", "BD", "EG", "BT", "YY", "F2")
  )
  expect_equal(
    output$simple_lifestage,
    c(
      "adult", "adult", NA_character_, "els", NA_character_, "juvenile",
      NA_character_
    )
  )
})

test_that("read in actual lifestage codes file and check message", {
  lifestage_file_path <- system.file(
    "extdata/lifestage-codes.csv",
    package = "wqbench"
  )

  db_lifestage_codes <- data.frame(
    code = c("--", "AD", "BD", "EG", "BT", "YY", "F2"),
    description = c("unspecified", "adult", "bud", "egg", "boot", "young", "F1 gen")
  )

  expect_message(
    expect_message(
      read_lifestage(lifestage_file_path, db_lifestage_codes)
    ),
    regexp = "'AL', 'BL', 'CS', 'EL', 'EM', 'EX', 'EY', 'F0', \\.\\.\\., 'ZY'"
  )
})
