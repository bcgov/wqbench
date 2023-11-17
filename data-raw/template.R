## code to prepare `template` dataset goes here

path <- system.file(
  package = "wqbench",
  "template/template-data.csv"
)

template <- readr::read_csv(path)
usethis::use_data(template, overwrite = TRUE)
