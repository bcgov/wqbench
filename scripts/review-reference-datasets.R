library(tidyverse)

# need to pull in raw database data
# update the file to the most recent version of the database stored locally on your computer
database <- "~/Ecotoxicology/ecotox_db/ecotox_ascii_12_14_2023.sqlite"

con <- DBI::dbConnect(
  RSQLite::SQLite(),
  database
)

db_conc_unit_codes <- DBI::dbReadTable(con, "concentration_unit_codes") |>
  mutate(
    add_new_multipler = NA_real_,
    add_new_unit = NA_character_
  ) |>
  tibble()

DBI::dbDisconnect(con)

# generate files for review
write_csv(
  db_conc_unit_codes,
  file.path(
    "~",
    "Poisson",
    "Data",
    "wqbench",
    format(Sys.Date(), "%Y"),
    "review",
    paste0(Sys.Date(), "-concentration-conversion-review", ".csv")
  ),
  na = ""
)

# Duration Unit -----------------------------------------------------------

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


# Lifestage Codes ---------------------------------------------------------

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


# Trophic Groups ----------------------------------------------------------

library(tidyverse)

# need to pull in raw database data
# update the file to the most recent version of the database stored locally on your computer
database <- "~/Ecotoxicology/ecotox_db/ecotox_ascii_12_14_2023.sqlite"

con <- DBI::dbConnect(
  RSQLite::SQLite(),
  database
)

db_species <- DBI::dbReadTable(con, "species") |>
  mutate(
    class = str_squish(class),
    tax_order = str_squish(tax_order)
  ) |>
  tibble()

DBI::dbDisconnect(con)

missing_species <- 
  db_species |>
  filter(is.na(trophic_group) | is.na(ecological_group))

missing_groups <- 
  missing_species |>
  select(phylum_division, class, tax_order, family) |>
  distinct() |>
  mutate(
    phylum_division = str_squish(phylum_division),
    class = str_squish(class),
    tax_order = str_squish(tax_order),
    family = str_squish(family),
    add = NA_character_,
    across(c(phylum_division, class, tax_order, family), ~ na_if(.x, ""))
  ) |>
  filter(
    !(is.na(phylum_division) & is.na(class) & is.na(tax_order) & is.na(family))
  ) |>
  arrange(phylum_division, class, tax_order, family)

# generate files for review
write_csv(
  missing_species,
  file.path(
    "~",
    "Poisson",
    "Data",
    "wqbench",
    format(Sys.Date(), "%Y"),
    "review",
    paste0(Sys.Date(), "-species-not-coded-in-db", ".csv")
  ),
  na = ""
)

write_csv(
  missing_groups,
  file.path(
    "~",
    "Poisson",
    "Data",
    "wqbench",
    format(Sys.Date(), "%Y"),
    "review",
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
    "~",
    "Poisson",
    "Data",
    "wqbench",
    format(Sys.Date(), "%Y"),
    "review",
    paste0(Sys.Date(), "-trophic-group", ".csv")
  )
)

