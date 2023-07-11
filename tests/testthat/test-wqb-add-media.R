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

test_that("values are coded into correct groupings", {
  db_media_type <- data.frame(
    code = c("--", "AGR", "FW", "SW", "NR", "SLG", "MAN"),
    description = c(
      "Unspecified", "Agar", "Fresh water", "Salt water",
      "Not reported", "Sludge", "Manure"
    )
  )

  output <- combine_media(db_media_type)

  expect_equal(
    output$media_type_group,
    c(
      "not reported", "not reported", "fresh water", "salt water",
      "not reported", "not reported", "not reported"
    )
  )
})

test_that("empty media type returns no rows", {
  db_media_type <- data.frame(
    code = character(),
    description = character()
  )
  output <- combine_media(db_media_type)
  expect_equal(
    nrow(output),
    0L
  )
})
