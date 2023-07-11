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

test_that("create sqlite file and it is not empty", {
  withr::defer(unlink(write_folder, recursive = TRUE))
  write_folder <- withr::local_tempdir()
  wqb_create_epa_ecotox(
    folder_path = write_folder,
    data_path = system.file("data-raw/sample/ecotox", package = "wqbenchdata"),
    quiet = TRUE,
    ask = FALSE
  )
  expect_true(
    "ecotox.sqlite" %in% list.files(write_folder)
  )
  expect_true(
    file.info(file.path(write_folder, "ecotox.sqlite"))$size > 0
  )
})

test_that("check message is turned on when quiet set to FALSE", {
  withr::defer(unlink(write_folder, recursive = TRUE))
  write_folder <- withr::local_tempdir()
  suppressMessages(
    expect_message(
      expect_message(
        wqb_create_epa_ecotox(
          folder_path = write_folder,
          data_path = system.file(
            "data-raw/sample/ecotox",
            package = "wqbenchdata"
          ),
          quiet = FALSE,
          ask = FALSE
        ),
        regexp = "Creating SQLite database"
      ),
      regexp = "Adding:.*"
    )
  )
})
