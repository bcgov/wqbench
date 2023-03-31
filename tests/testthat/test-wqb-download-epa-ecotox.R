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

test_that("error when version beyond 4 passed", {
  expect_error(
    wqb_download_epa_ecotox(version = 5),
    regexp = "`version` must be between 1 and 4, not 5\\."
  )
})

test_that("error when version lower then 1 is passed", {
  expect_error(
    wqb_download_epa_ecotox(version = 0),
    regexp = "`version` must be between 1 and 4, not 0\\."
  )
})

test_that("folder is downloaded", {
  skip_if_offline("cfpub.epa.gov")
  skip_if_testing_quick()
  
  withr::defer(unlink(write_folder, recursive = TRUE))
  write_folder <- withr::local_tempdir()
  expect_message(
    wqb_download_epa_ecotox(
      file_path = write_folder,
      ask = FALSE
    ),
    regexp = "Downloading..."
  )
  expect_true(
    length(list.files(write_folder)) == 1
  )
  expect_true(
    "tests.txt" %in% basename(list.files(write_folder, recursive = TRUE))
  )
})
