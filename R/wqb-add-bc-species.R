# Copyright 2023 Province of British Columbia
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

#' Add BC Species
#'
#' Read in a list of British Columbia species and add a column to the `species`
#' table in the database to indicate if the species is present in British
#' Columbia.
#'
#' @param database A string to the location of the database.
#' @param quiet Turn off message when quiet set to TRUE.
#' @return Invisible data frame
#' @export
#' @details The BC species data is contained in a csv file in the extdata folder
#'   of the package. This file can be edited by adding new species or removing
#'   species. Do not add new columns, rename columns or rename the file. The
#'   file must only contain a single column named `latin_name`.
#'
#'   The `latin_name` column must consist of the genus and species separated by
#'   a space. The `latin_name` column in the bc-species.csv file is matched to
#'   the `latin_name` column in the species table of the database. A new column
#'   `species_present_in_bc` is added to the species table that codes each
#'   species as TRUE if it matches a value in the bc-species.csv or FALSE if
#'   there is no match.
#'
#' @examples
#' \dontrun{
#' bc_species <- wqb_add_bc_species(
#'   database = "ecotox_ascii_09_15_2022.sqlite"
#' )
#'
#' bc_species <- wqb_add_bc_species(
#'   database = "ecotox_db/ecotox_ascii_09_15_2022.sqlite"
#' )
#' }
wqb_add_bc_species <- function(database, quiet = FALSE) {
  chk::chk_file(database)
  chk::chk_ext(database, "sqlite")

  # read in species from db
  on.exit(DBI::dbDisconnect(con))
  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    database
  )
  db_species <- DBI::dbReadTable(con, "species")

  if ("species_present_in_bc" %in% colnames(db_species)) {
    stop(
      "British Columbia species have already been added to the database"
    )
  }
  # read in bc species
  bc_species_file_path <- system.file(
    "extdata/bc-species.csv",
    package = "wqbench"
  )

  species_british_columbia <- read_bc_species(bc_species_file_path, db_species)

  DBI::dbExecute(
    con,
    paste0(
      "CREATE TABLE species_british_columbia ",
      "(", paste(colnames(species_british_columbia), collapse = ", "),
      ", PRIMARY KEY (species_number))"
    )
  )

  DBI::dbWriteTable(
    con,
    "species_british_columbia",
    value = species_british_columbia,
    append = TRUE,
    row.names = FALSE
  )
  if (!quiet) {
    message("Adding: BC species to species table")
  }
  DBI::dbExecute(con, "DROP TABLE species;")
  DBI::dbExecute(
    con,
    "ALTER TABLE species_british_columbia
  RENAME TO species;"
  )

  invisible(species_british_columbia)
}

#' Combine BC Species and DB Species
#'
#' Internal to allow for testing
#'
#' @param bc_species A data frame
#' @param db_species A data frame
#' @return A data frame
#'
#' @examples
#' \dontrun{
#' data <- combine_bc_species(bc_species, db_species)
#' }
combine_bc_species <- function(bc_species, db_species) {
  bc_species <- bc_species |>
    dplyr::mutate(
      latin_name = stringr::str_squish(.data$latin_name),
      species_present_in_bc = TRUE
    )

  # add bc species flag to species table
  species_british_columbia <- db_species |>
    dplyr::mutate(latin_name = stringr::str_squish(.data$latin_name)) |>
    dplyr::left_join(bc_species, by = "latin_name") |>
    dplyr::mutate(
      species_present_in_bc = tidyr::replace_na(.data$species_present_in_bc, FALSE)
    ) |>
    tibble::tibble()

  species_british_columbia
}

#' Read in BC Species CSV
#'
#' Internal to allow for testing
#'
#' @param bc_species_file_path A file path
#' @param db_species A data frame
#' @return A data frame
#'
#' @examples
#' \dontrun{
#' data <- read_bc_species(bc_species_file_path, db_species)
#' }
read_bc_species <- function(bc_species_file_path, db_species) {
  bc_species <- readr::read_csv(bc_species_file_path, show_col_types = FALSE)
  chk::check_data(
    bc_species,
    list(
      latin_name = ""
    )
  )

  species_british_columbia <- combine_bc_species(bc_species, db_species)

  species_british_columbia
}
