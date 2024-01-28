library(tidyverse)

# need to pull in raw database data
# update the file to the most recent version of the database stored locally on your computer
database <- "~/Ecotoxicology/ecotox_db/ecotox_ascii_12_14_2023.sqlite"

con <- DBI::dbConnect(
  RSQLite::SQLite(),
  database
)

db_duration_unit_codes <- DBI::dbReadTable(con, "duration_unit_codes") |>
  mutate(
    add_new_multipler = NA_real_,
    add_comments = NA_character_
  ) |>
  tibble()

DBI::dbDisconnect(con)

# generate files for review
write_csv(
  db_duration_unit_codes,
  file.path(
    "~",
    "Poisson",
    "Data",
    "wqbench",
    format(Sys.Date(), "%Y"),
    "review",
    paste0(Sys.Date(), "-duration-conversion-review", ".csv")
  ),
  na = ""
)
