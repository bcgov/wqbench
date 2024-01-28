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
    class = str_squish(.data$class),
    tax_order = str_squish(.data$tax_order)
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
