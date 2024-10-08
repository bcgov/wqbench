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

#' Add BC Water Quality Guidelines
#'
#' Read in the British Columbia water quality guidelines (wqg) and add a column
#' to the chemicals tables in the database to indicate if the chemical is
#' present in the British Columbia water quality guidelines.
#'
#' @param database A string to the location of the database.
#' @param quiet Turn off message when quiet set to TRUE.
#' @return Invisible data frame
#' @export
#' @details The wqg data is stored in the BC Data Catalogue.
#'
#'   The `CAS_number` column in the bc wqg data is matched to the `cas_number`
#'   column in the chemicals table of the database. A new column
#'   `present_in_bc_wqg` is added to the chemicals table that codes each
#'   chemical as TRUE if the chemical is present in wqg or FALSE if the chemical
#'   is not present in wqg.
#'
#' @examples
#' \dontrun{
#' chem_bc_wqg <- wqb_add_bc_wqg(
#'   database = "ecotox_ascii_09_15_2022.sqlite"
#' )
#'
#' chem_bc_wqg <- wqb_add_bc_wqg(
#'   database = "ecotox_db/ecotox_ascii_09_15_2022.sqlite"
#' )
#' }
wqb_add_bc_wqg <- function(database, quiet = FALSE) {
  chk::chk_file(database)
  chk::chk_ext(database, "sqlite")

  # read in chemicals from db
  on.exit(DBI::dbDisconnect(con))
  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    database
  )

  db_chemicals <- DBI::dbReadTable(con, "chemicals") |>
    dplyr::mutate(
      cas_number = as.character(.data$cas_number),
    ) |>
    tibble::tibble()

  if ("present_in_bc_wqg" %in% colnames(db_chemicals)) {
    stop(
      paste(
        "British Columbia water quality guideline flag has already been",
        "added to the database"
      )
    )
  }

  chemicals_bc_wqg <- read_bc_wqg(db_chemicals)

  # create db tables
  DBI::dbExecute(
    con,
    paste0(
      "CREATE TABLE chemicals_bc_wqg ",
      "(", paste(colnames(chemicals_bc_wqg), collapse = ", "),
      ", PRIMARY KEY (cas_number))"
    )
  )

  DBI::dbWriteTable(
    con,
    "chemicals_bc_wqg",
    value = chemicals_bc_wqg,
    append = TRUE,
    row.names = FALSE
  )
  if (!quiet) {
    message("Adding: BC WQG to chemicals table")
  }
  DBI::dbExecute(con, "DROP TABLE chemicals;")
  DBI::dbExecute(
    con,
    "ALTER TABLE chemicals_bc_wqg
  RENAME TO chemicals;"
  )

  invisible(chemicals_bc_wqg)
}

#' Combine BC Water Quality Guidelines with DB Chemicals
#'
#' Internal to allow for testing
#'
#' @param bc_wqg A data frame
#' @param db_chemicals A data frame
#'
#' @return A data frame
#' @examples
#' \dontrun{
#' data <- combine_bc_wqg(bc_wqg, db_chemicals)
#' }
combine_bc_wqg <- function(bc_wqg, db_chemicals) {
  bc_wqg <- bc_wqg |>
    dplyr::filter(.data$Media == "Water" & .data$Type == "Long-term chronic") |>
    dplyr::filter(.data$Use == "Aquatic Life - Freshwater" | .data$Use == "Aquatic Life - Marine" | .data$Use == "Aquatic Life - Estuarine") |>
    dplyr::select("CAS_number") |>
    dplyr::mutate(
      CAS_number = stringr::str_squish(.data$CAS_number),
      CAS_number = stringr::str_replace(.data$CAS_number, "^\\(", ""),
      CAS_number = stringr::str_replace(.data$CAS_number, "\\)$", ""),
      CAS_number = stringr::str_replace_all(.data$CAS_number, "\\-", ""),
      CAS_number = stringr::str_replace_all(.data$CAS_number, "[:alpha:]|[:space:]", ""),
      CAS_number = dplyr::na_if(.data$CAS_number, ""),
      present_in_bc_wqg = TRUE
    ) |>
    tidyr::drop_na("CAS_number") |>
    dplyr::rename(cas_number = "CAS_number") |>
    dplyr::distinct()

  # add bc wqg flag to chemicals table
  chemicals_bc_wqg <- db_chemicals |>
    dplyr::left_join(bc_wqg, by = "cas_number") |>
    dplyr::mutate(
      present_in_bc_wqg = tidyr::replace_na(.data$present_in_bc_wqg, FALSE)
    ) |>
    tibble::tibble()
  chemicals_bc_wqg
}

#' Read BC Water Quality Guidelines
#'
#' Internal to allow for testing
#'
#' @param db_chemicals A data frame
#'
#' @return A data frame
#' @examples
#' \dontrun{
#' chemicals_bc_wqg <- read_bc_wqg(db_chemicals)
#' }
read_bc_wqg <- function(db_chemicals) {
  # read in bc wqg from bcdata
  bc_wqg <- suppressMessages(
    bcdata::bcdc_get_data(
      record = "85d3990a-ec0a-4436-8ebd-150de3ba0747",
      resource = "6f32a85b-a3d9-44c3-9a14-15175eba25b6"
    )
  )


  chk::check_data(
    bc_wqg,
    list(
      `CAS_number` = character()
    )
  )

  data <- combine_bc_wqg(bc_wqg, db_chemicals)
  data
}
