# Copyright 2024 Province of British Columbia
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

rm(list = ls())
library(tidyverse)

# Setup -------------------------------------------------------------------

# need to pull in raw database data
# update the file to the most recent version of the database stored locally on your computer
list.files("~/Ecotoxicology/ecotox_db/", pattern = "\\.sqlite")
database <- "~/Ecotoxicology/ecotox_db/ecotox_ascii_09_12_2024.sqlite"

con <- DBI::dbConnect(
  RSQLite::SQLite(),
  database
)

file_save_loc <-
  file.path(
    "~",
    "Poisson",
    "Data",
    "wqbench",
    format(Sys.Date(), "%Y"),
    "review",
    "to-be-reviewed"
  )

dir.create(file_save_loc, recursive = TRUE)

# Lifestage Codes ---------------------------------------------------------

db_results <- DBI::dbReadTable(con, "results")
db_tests <- DBI::dbReadTable(con, "tests")
db_species <- DBI::dbReadTable(con, "species")

db_lifestage_codes <- DBI::dbReadTable(con, "lifestage_codes") |>
  dplyr::mutate(
    code = stringr::str_squish(code)
  ) |>
  tibble()

only_fish_amphibian_life_stages <-
  db_results |>
  dplyr::left_join(db_tests, by = "test_id") |>
  dplyr::filter(organism_habitat == "Water") |> # filter to only water  (aquatic) tests
  dplyr::left_join(db_species, by = "species_number") |>
  dplyr::left_join(db_lifestage_codes, by = c("organism_lifestage" = "code")) |>
  dplyr::filter(trophic_group %in% c("Fish", "Amphibian")) |>
  dplyr::select(organism_lifestage) |>
  dplyr::distinct() |>
  dplyr::mutate("fish_amphibian_flag" = TRUE)

life_stage_review <-
  db_lifestage_codes %>%
  dplyr::left_join(only_fish_amphibian_life_stages, by = c("code" = "organism_lifestage"))

# generate files for review
write_csv(
  life_stage_review,
  file.path(
    file_save_loc,
    paste0(Sys.Date(), "-lifestage-code-review", ".csv")
  ),
  na = ""
)

# Clean Up ----------------------------------------------------------------

DBI::dbDisconnect(con)

dir.create(
  file.path(
    "~",
    "Poisson",
    "Data",
    "wqbench",
    format(Sys.Date(), "%Y"),
    "review",
    "completed"
  ),
  recursive = TRUE
)
