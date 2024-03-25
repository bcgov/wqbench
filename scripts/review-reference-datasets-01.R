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
database <- "~/Ecotoxicology/ecotox_db/ecotox_ascii_12_14_2023.sqlite"

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

# Concentration Units -----------------------------------------------------

db_conc_unit_codes <- DBI::dbReadTable(con, "concentration_unit_codes") |>
  tibble()

# generate files for review
write_csv(
  db_conc_unit_codes,
  file.path(
    file_save_loc,
    paste0(Sys.Date(), "-concentration-conversion-review", ".csv")
  ),
  na = ""
)

# Duration Unit -----------------------------------------------------------

db_duration_unit_codes <- DBI::dbReadTable(con, "duration_unit_codes") |>
  tibble()

# generate files for review
write_csv(
  db_duration_unit_codes,
  file.path(
    file_save_loc,
    paste0(Sys.Date(), "-duration-conversion-review", ".csv")
  ),
  na = ""
)

# Trophic Groups ----------------------------------------------------------

db_tests <- DBI::dbReadTable(con, "tests")

db_tests_aquatic <- 
  db_tests |>
  select(species_number, organism_habitat) |>
  distinct() |>
  filter(organism_habitat == "Water") |>
  tibble()
  
db_species <- DBI::dbReadTable(con, "species") |>
  mutate(
    phylum_division = str_squish(phylum_division),
    class = str_squish(class),
    tax_order = str_squish(tax_order),
    family = str_squish(family)
  ) |>
  tibble()

  # this filters to only the aquatic tests
db_species_aqutic <- 
  db_species |>
  left_join(db_tests_aquatic, by = "species_number") |>
  filter(organism_habitat == "Water")

missing_species <- 
  db_species_aqutic |>
  filter(is.na(trophic_group) | is.na(ecological_group)) |>
  select(phylum_division, class, tax_order, family) |>
  distinct() |>
  mutate(
    phylum_division = str_squish(phylum_division),
    class = str_squish(class),
    tax_order = str_squish(tax_order),
    family = str_squish(family),
    across(c(phylum_division, class, tax_order, family), ~ na_if(.x, ""))
  ) |>
  filter(
    !(is.na(phylum_division) & is.na(class) & is.na(tax_order) & is.na(family))
  ) |>
  arrange(phylum_division, class, tax_order, family)

# generate files for review
write_csv(
  db_species_aqutic,
  file.path(
    file_save_loc,
    paste0(Sys.Date(), "-species-coded-in-db", ".csv")
  ),
  na = ""
)

write_csv(
  missing_species,
  file.path(
    file_save_loc,
    paste0(Sys.Date(), "-missing-trophic-group-review", ".csv")
  ),
  na = ""
)

file.copy(
  from = system.file(
    "extdata/trophic-group.csv",
    package = "wqbench"
  ),
  to = file.path(
    file_save_loc,
    paste0(Sys.Date(), "-trophic-group", ".csv")
  )
)

# Lifestage Codes ---------------------------------------------------------

db_tests_aq_lifestage_sp <- 
  db_tests |>
  select(species_number, organism_habitat, organism_lifestage) |>
  distinct() |>
  filter(organism_habitat == "Water") |>
  left_join(db_species, by = "species_number") |>
  select(organism_lifestage, trophic_group) |>
  filter(trophic_group %in% c("Amphibian", "Fish")) |>
  distinct() |>
  tibble() 


db_lifestage_codes <- DBI::dbReadTable(con, "lifestage_codes") |>
  dplyr::mutate(
    code = stringr::str_squish(code)
  ) |>
  tibble()

lifestage_codes_fish_amphibian <- 
  db_lifestage_codes |>
  left_join(db_tests_aq_lifestage_sp, by = c("code" = "organism_lifestage"))

# generate files for review
write_csv(
  lifestage_codes_fish_amphibian,
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
