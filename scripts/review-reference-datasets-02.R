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

# Lifestage Codes ---------------------------------------------------------

db_lifestage_codes <- DBI::dbReadTable(con, "lifestage_codes") |>
  dplyr::mutate(
    code = stringr::str_squish(code)
  ) |>
  tibble()

# generate files for review
write_csv(
  db_lifestage_codes,
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
