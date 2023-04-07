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

#' Add Duration Unit Conversation Values
#'
#' Read in the duration-conversion file and code unit values that can be
#' converted and which ones will be removed. Two columns will be added to the
#' duration_unit_codes table in the database to indicate which units are being
#' kept and the conversion factor. Currently duration units are converted to
#' hours.
#'
#' @param database A string to the location of the database.
#' @param quiet Turn off message when quiet set to TRUE.
#' @return Invisible data frame
#' @export
#' @details The list of units to be converted are contained in a csv file in the
#'   extdata folder of the package. The csv file can be edited by adding or
#'   removing rows.  To add new rows get the `code` and `description` values
#'   from the `duration_unit_codes` table in the database and paste them into
#'   the csv file.
#'
#'   Do not add new columns, rename columns or rename the file. The file must
#'   only contain the columns: `code`, `description`, `duration_units_to_keep`
#'   and `duration_value_multiplier_to_hours`.
#'
#'   The `code` values in the duration-conversion file are matched to the `code`
#'   values in the `duration_unit_codes` table in the database.
#'
#'   The `duration_units_to_keep` column indicates which units are being
#'   converted and which are being removed because they can not be converted or
#'   are not in the aquatic portion of the data. The
#'   `duration_value_multiplier_to_hours` column contains the value need to
#'   convert the unit into hours.
#'
#' @examples
#' \dontrun{
#' duration_unit_code_standardization <- wqb_add_duration_conversions(
#'  database = "ecotox_ascii_09_15_2022.sqlite"
#' )
#'
#' duration_unit_code_standardization <- wqb_add_duration_conversions(
#'  database = "ecotox_db/ecotox_ascii_09_15_2022.sqlite"
#' )
#' }
wqb_add_duration_conversions <- function(database, quiet = FALSE) {
  chk::chk_file(database)
  chk::chk_ext(database, "sqlite")
  
  # read in from db
  on.exit(DBI::dbDisconnect(con))
  con  <- DBI::dbConnect(
    RSQLite::SQLite(), 
    database
  )
  
  db_duration_unit_codes <- DBI::dbReadTable(con, "duration_unit_codes") |>
    dplyr::mutate(
      code = stringr::str_squish(.data$code),
    ) |>
    tibble::tibble()
  
  if ("duration_value_multiplier_to_hours" %in% colnames(db_duration_unit_codes)) {
    stop(
      paste(
        "Value multiplier to hours has already been", 
        "added to the database"
      )
    )
  }
  
  if ("duration_units_to_keep" %in% colnames(db_duration_unit_codes)) {
    stop(
      paste(
        "duration_units_to_keep has already been added to the database"
      )
    )
  }
  
  duration_std_file_path <- system.file(
    "extdata/duration-conversion.csv",
    package = "wqbench"
  )

  duration_unit_codes_std <- read_duration_conversions(
    duration_std_file_path, db_duration_unit_codes
  )
  
  # create db tables
  DBI::dbExecute(
    con,
    paste0(
      "CREATE TABLE duration_unit_codes_std ",
      "(", paste(colnames(duration_unit_codes_std), collapse = ", "),
      ", PRIMARY KEY (code))"
    )
  )
  
  DBI::dbWriteTable(
    con,
    "duration_unit_codes_std",
    value = duration_unit_codes_std,
    append = TRUE,
    row.names = FALSE
  )
  if (!quiet) {
    message("Adding: duration conversions to duration_unit_codes table")
  }
  DBI::dbExecute(con, "DROP TABLE duration_unit_codes;")
  DBI::dbExecute(
    con,
    "ALTER TABLE duration_unit_codes_std
  RENAME TO duration_unit_codes;"
  )
  
  invisible(duration_unit_codes_std)
}

#' Combine Duration Unit Conversation Values and DB duration unit
#' 
#' Internal to allow for testing
#'
#' @return Invisible data frame
#'
#' @examples
#' \dontrun{
#' duration_unit_codes_std <- combine_duration_conversions(
#'   duration_std, db_duration_unit_codes
#' )
#' }
combine_duration_conversions <- function(duration_std, db_duration_unit_codes) {
  duration_std <- duration_std |>
    dplyr::mutate(
      code = dplyr::if_else(
        .data$description == "Pretreatment, time unknown" & .data$code == "#NAME?",
        "-X",
        .data$code
      ),
      code = stringr::str_squish(.data$code) 
    ) |>
    dplyr::select("code", "duration_units_to_keep", "duration_value_multiplier_to_hours")
  
  # print out name of any codes that don't match the db ones
  dont_match <- !(duration_std$code %in% db_duration_unit_codes$code)
  if (any((dont_match))) {
    message("Value(s) do not match code(s) in `duration_unit_codes` table in ECOTOX database:")
    message(chk::cc(duration_std$code[dont_match]))
  }
  
  # add conversion values to the duration table
  duration_unit_codes_std <- db_duration_unit_codes |>
    dplyr::left_join(duration_std, by = "code") |>
    tibble::tibble() 
}

#' Read Duration Unit Conversation
#' 
#' Internal to allow for testing
#'
#' @return Invisible data frame
#'
#' @examples
#' \dontrun{
#' duration_unit_codes_std <- read_duration_conversions(
#'  duration_std_file_path, db_duration_unit_codes
#' )
#' }
read_duration_conversions <- function(duration_std_file_path,
                                      db_duration_unit_codes) {
  duration_std <- readr::read_csv(
    duration_std_file_path, show_col_types = FALSE
  ) 
  chk::check_data(
    duration_std, 
    list(
      code = c("", NA),
      description = c("", NA),
      duration_units_to_keep = TRUE,
      duration_value_multiplier_to_hours = c(1, NA)
    )
  )
  
  duration_unit_codes_std <- combine_duration_conversions(
    duration_std, db_duration_unit_codes
  )
  duration_unit_codes_std
}
