library(tidyverse)

# need to pull in raw database data
# update the file to the most recent version of the database stored locally on your computer
database <- "~/Ecotoxicology/ecotox_db/ecotox_ascii_12_14_2023.sqlite"

con <- DBI::dbConnect(
  RSQLite::SQLite(),
  database
)

db_lifestage_codes <- DBI::dbReadTable(con, "lifestage_codes") |>
  dplyr::mutate(
    code = stringr::str_squish(code),
    add = NA_character_
  ) |>
  tibble()

DBI::dbDisconnect(con)

# generate files for review
write_csv(
  db_lifestage_codes,
  file.path(
    "~",
    "Poisson",
    "Data",
    "wqbench",
    format(Sys.Date(), "%Y"),
    "review",
    paste0(Sys.Date(), "-lifestage-code-review", ".csv")
  ),
  na = ""
)
