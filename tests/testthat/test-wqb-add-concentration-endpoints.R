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

test_that("combine conc endpoints by codes", {
  endpoint_concentration_pick <- data.frame(
    code = c("EC18", "LC20", "MATC", "NOEC")
  )

  db_endpoint_codes <- data.frame(
    code = c("EC18", "LC20", "MATC", "NOEC", "--", "BCF", "BMC20"),
    description = c(
      "Effective conc", "Lethal conc", "Maximum acceptable",
      "No-observable", "Bioconc", "", "Benchmark"
    )
  )

  output <- combine_concentration_endpoints(
    endpoint_concentration_pick, db_endpoint_codes
  )

  expect_equal(
    output$code,
    c("EC18", "LC20", "MATC", "NOEC", "--", "BCF", "BMC20")
  )
  expect_equal(
    output$concentration_flag,
    c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  )
})

test_that("combines when spaced added to entered conc endpoints by codes", {
  endpoint_concentration_pick <- data.frame(
    code = c(" EC18", " LC20 ", "MATC  ", " NOEC")
  )

  db_endpoint_codes <- data.frame(
    code = c("EC18", "LC20", "MATC", "NOEC", "--", "BCF", "BMC20"),
    description = c(
      "Effective conc", "Lethal conc", "Maximum acceptable",
      "No-observable", "Bioconc", "", "Benchmark"
    )
  )

  output <- combine_concentration_endpoints(
    endpoint_concentration_pick, db_endpoint_codes
  )

  expect_equal(
    output$code,
    c("EC18", "LC20", "MATC", "NOEC", "--", "BCF", "BMC20")
  )
  expect_equal(
    output$concentration_flag,
    c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  )
})

test_that("message when values are present in inputted but not master list", {
  endpoint_concentration_pick <- data.frame(
    code = c("EX18", "LC20 ", "ZZZ", "NOEC")
  )

  db_endpoint_codes <- data.frame(
    code = c("EC18", "LC20", "MATC", "NOEC", "--", "BCF", "BMC20"),
    description = c(
      "Effective conc", "Lethal conc", "Maximum acceptable",
      "No-observable", "Bioconc", "", "Benchmark"
    )
  )

  expect_message(
    expect_message(
      combine_concentration_endpoints(
        endpoint_concentration_pick, db_endpoint_codes
      ),
      regexp = "Value\\(s\\) do not match code\\(s\\) in `endpoint_codes` table in ECOTOX database:"
    ),
    regexp = "'EX18', 'ZZZ'"
  )
})

test_that("mismatches don't cause issue with matches", {
  endpoint_concentration_pick <- data.frame(
    code = c("EX18", "LC20 ", "ZZZ", "NOEC")
  )

  db_endpoint_codes <- data.frame(
    code = c("EC18", "LC20", "MATC", "NOEC", "--", "BCF", "BMC20"),
    description = c(
      "Effective conc", "Lethal conc", "Maximum acceptable",
      "No-observable", "Bioconc", "", "Benchmark"
    )
  )

  output <- suppressMessages(combine_concentration_endpoints(
    endpoint_concentration_pick, db_endpoint_codes
  ))
  expect_equal(
    output$code,
    c("EC18", "LC20", "MATC", "NOEC", "--", "BCF", "BMC20")
  )
  expect_equal(
    output$concentration_flag,
    c(FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE)
  )
})

test_that("empty conc means all coded as FALSE", {
  endpoint_concentration_pick <- data.frame(
    code = character()
  )
  db_endpoint_codes <- data.frame(
    code = c("EC18", "LC20", "MATC", "NOEC", "--", "BCF", "BMC20"),
    description = c(
      "Effective conc", "Lethal conc", "Maximum acceptable",
      "No-observable", "Bioconc", "", "Benchmark"
    )
  )
  output <- combine_concentration_endpoints(
    endpoint_concentration_pick, db_endpoint_codes
  )
  expect_equal(
    output$code,
    c("EC18", "LC20", "MATC", "NOEC", "--", "BCF", "BMC20")
  )
  expect_equal(
    output$concentration_flag,
    c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  )
})

test_that("all missing conc endpoints means all coded as FALSE", {
  endpoint_concentration_pick <- data.frame(
    code = NA_character_
  )
  db_endpoint_codes <- data.frame(
    code = c("EC18", "LC20", "MATC", "NOEC", "--", "BCF", "BMC20"),
    description = c(
      "Effective conc", "Lethal conc", "Maximum acceptable",
      "No-observable", "Bioconc", "", "Benchmark"
    )
  )
  output <- suppressMessages(combine_concentration_endpoints(
    endpoint_concentration_pick, db_endpoint_codes
  ))
  expect_equal(
    output$code,
    c("EC18", "LC20", "MATC", "NOEC", "--", "BCF", "BMC20")
  )
  expect_equal(
    output$concentration_flag,
    c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  )
})

test_that("empty db table means no rows are returned", {
  endpoint_concentration_pick <- data.frame(
    code = c(" EC18", " LC20 ", "MATC  ", " NOEC")
  )
  db_endpoint_codes <- data.frame(
    code = character(),
    description = character()
  )
  output <- suppressMessages(combine_concentration_endpoints(
    endpoint_concentration_pick, db_endpoint_codes
  ))
  expect_equal(
    nrow(output),
    0L
  )
  expect_equal(
    colnames(output),
    c("code", "description", "concentration_flag")
  )
})

test_that("read in actual conc endpoint file", {
  conc_endpoints_file_path <- system.file(
    "extdata/concentration-endpoints.csv",
    package = "wqbench"
  )
  db_endpoint_codes <- data.frame(
    code = c("EC18", "LC20", "MATC", "NOEC", "--", "BCF", "BMC20"),
    description = c(
      "Effective conc", "Lethal conc", "Maximum acceptable",
      "No-observable", "Bioconc", "", "Benchmark"
    )
  )
  output <- suppressMessages(read_concentration_endpoints(
    conc_endpoints_file_path, db_endpoint_codes
  ))
  expect_equal(
    output$code,
    c("EC18", "LC20", "MATC", "NOEC", "--", "BCF", "BMC20")
  )
  expect_equal(
    output$concentration_flag,
    c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  )
})

test_that("read in actual conc endpoint file and check message", {
  conc_endpoints_file_path <- system.file(
    "extdata/concentration-endpoints.csv",
    package = "wqbench"
  )
  db_endpoint_codes <- data.frame(
    code = c("EC18", "LC20", "MATC", "NOEC", "--", "BCF", "BMC20"),
    description = c(
      "Effective conc", "Lethal conc", "Maximum acceptable",
      "No-observable", "Bioconc", "", "Benchmark"
    )
  )

  expect_message(
    expect_message(
      read_concentration_endpoints(
        conc_endpoints_file_path, db_endpoint_codes
      )
    ),
    regexp = "'EC05', 'EC06', 'EC07', 'EC08', 'EC09', 'EC10', 'EC11', 'EC12', ..., 'NOEC\\*'"
  )
})
