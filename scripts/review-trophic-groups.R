library(tidyverse)

# need to pull in raw database data
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

db_trophic_groups <- 
  db_species |>
  select(class, tax_order) |>
  mutate(
    class = str_squish(class),
    tax_order = str_squish(tax_order),
    class = na_if(class, ""),
    tax_order = na_if(tax_order, ""),
    in_db = TRUE
  ) |>
  filter(!(is.na(class) & is.na(tax_order))) |>
  rename(order = tax_order) |>
  distinct() 

# need to pull in compiled list
trophic_groups_file_path <- system.file(
  "extdata/trophic-group.csv",
  package = "wqbench"
)

trophic_groups <- readr::read_csv(
  trophic_groups_file_path,
  show_col_types = FALSE
) |>
  mutate(
    in_reference_data = TRUE
  )

# compare lists to see variation 
trophic_group_list <- 
  db_trophic_groups |>
  full_join(trophic_groups, by = c("class", "order")) |>
  arrange(class, order) 

write_csv(
  trophic_group_list,
  file.path(
    "~",
    "Poisson",
    "Data",
    "wqbench",
    format(Sys.Date(), "%Y"),
    "review",
    "trophic_group_review.csv"
  ),
  na = ""
)

write_csv(
  db_species,
  file.path(
    "~",
    "Poisson",
    "Data",
    "wqbench",
    format(Sys.Date(), "%Y"),
    "review",
    "full_list_database_species.csv"
  ),
  na = ""
)
