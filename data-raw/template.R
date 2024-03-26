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

## code to prepare `template` dataset goes here

path <- system.file(
  package = "wqbench",
  "template/template-data-data.csv"
)

template <- readr::read_csv(path)
usethis::use_data(template, overwrite = TRUE)

# add extra page to template file
endpoints_path <- system.file(
  "extdata/concentration-endpoints.csv",
  package = "wqbench"
)

# error if inst/template not present
if (!file.exists("inst/template/template-data.xlsx")) {
  warning("Stop and find out why template file missing")
}

# need to read in whole work book then add each sheet back (just how xlsx files work)
sheet_1 <- readr::read_csv(
  "inst/template/template-data-data.csv"
)[0, -1]

sheet_2 <- readr::read_csv(
  "inst/template/template-data-instructions.csv"
)

wb <- openxlsx::createWorkbook()

# add sheets to the workbook
openxlsx::addWorksheet(wb, "data")
openxlsx::addWorksheet(wb, "instructions")

# write data to the sheets
openxlsx::writeData(wb, sheet = "data", x = sheet_1)
openxlsx::writeData(wb, sheet = "instructions", x = sheet_2)

# export the file
openxlsx::saveWorkbook(wb, "inst/template/template-data.xlsx", overwrite = TRUE)
