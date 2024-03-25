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

db_species <- DBI::dbReadTable(con, "species") |>
  mutate(
    class = str_squish(class),
    tax_order = str_squish(tax_order)
  ) |>
  tibble()

missing_species <- 
  db_species |>
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
  db_species,
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
