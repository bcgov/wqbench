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

#' Add Concentration Unit Conversation Values
#'
#' Read in the concentration-conversion file and code units that can be
#' converted and which ones will be removed. Three columns will be added to the
#' concentration_unit_codes table in the database, indicate which units are
#' being kept, the conversion factor, and the units it is being converted to. 
#' Currently units are converted to mg/L or equivalent.
#'
#' @param database A string to the location of the database.
#' @param quiet Turn off message when quiet set to TRUE.
#' @return Invisible data frame
#' @export
#' @details The list of units to be converted are contained in a csv file in the
#'   extdata folder of the package. The csv file can be edited by adding or
#'   removing rows.  To add new rows get the `code` and `description` values
#'   from the `concentration_unit_codes` table in the database and paste them
#'   into the csv file.
#'
#'   Do not add new columns, rename columns or rename the file. The file must
#'   only contain the columns: `code`, `description`, `conc_conversion_flag`,
#'   `conc_conversion_value_multiplier` and `conc_conversion_unit`.
#'   
#'   The `conc_conversion_flag` column indicates which units are being converted 
#'   and which are being removed because they can not be converted or are not in 
#'   the aquatic portion of the data.The `conc_conversion_value_multiplier` 
#'   column contains the value need to convert the unit into the unit listed in 
#'   the `conc_conversion_unit` column.
#'
#'   The `code` values in the concentration-conversion file are matched to the
#'   `code` values in the `concentration_unit_codes` table in the database.
#'   
#' @examples
#' \dontrun{
#' concentration_unit_code_standardization <- wqb_add_conc_conversions(
#'  database = "ecotox_ascii_09_15_2022.sqlite"
#' )
#'
#' concentration_unit_code_standardization <- wqb_add_conc_conversions(
#'  database = "ecotox_db/ecotox_ascii_09_15_2022.sqlite"
#' )
#' }
wqb_add_conc_conversions <- function(database, quiet = FALSE) {
  chk::chk_file(database)
  chk::chk_ext(database, "sqlite")
  
  # read in from db
  on.exit(DBI::dbDisconnect(con))
  con  <- DBI::dbConnect(
    RSQLite::SQLite(), 
    database
  )
  
  db_concentration_unit_codes <- DBI::dbReadTable(
    con, "concentration_unit_codes"
  ) |>
    dplyr::mutate(
      code = stringr::str_squish(.data$code),
    ) |>
    tibble::tibble()
  
  if ("conc_conversion_flag" %in% colnames(db_concentration_unit_codes)) {
    stop(
      paste(
        "Concentration converstion flag has already been added to the database"
      )
    )
  }
  
  if ("conc_conversion_value_multiplier" %in% colnames(db_concentration_unit_codes)) {
    stop(
      paste(
        "Concentration conversion value multiplier has already been added to",
        "the database"
      )
    )
  }
  
  if ("conc_conversion_unit" %in% colnames(db_concentration_unit_codes)) {
    stop(
      paste(
        "Concentration conversion units has already been added to the database"
      )
    )
  }
  
  concentration_std_file_path <- system.file(
    "extdata/concentration-conversion.csv",
    package = "wqbench"
  )
  
  concentration_unit_codes_std <-  read_conc_conversions(
    concentration_std_file_path, db_concentration_unit_codes
  )
  
  # create db tables
  DBI::dbExecute(
    con,
    paste0(
      "CREATE TABLE concentration_unit_codes_std ",
      "(", paste(colnames(concentration_unit_codes_std), collapse = ", "),
      ", PRIMARY KEY (code))"
    )
  )
  DBI::dbWriteTable(
    con,
    "concentration_unit_codes_std",
    value = concentration_unit_codes_std,
    append = TRUE,
    row.names = FALSE
  )
  if (!quiet) {
    message("Adding: concentration conversions to concentration_unit_codes table")
  }
  DBI::dbExecute(con, "DROP TABLE concentration_unit_codes;")
  DBI::dbExecute(
    con,
    "ALTER TABLE concentration_unit_codes_std
  RENAME TO concentration_unit_codes;"
  )
  
  invisible(concentration_unit_codes_std)
}

#' Combine Concentration Unit Conversation Values with DB Concentration Unit
#' 
#' Internal to allow for testing
#'
#' @param concentration_std A data frame
#' @param db_concentration_unit_codes A data frame
#'
#' @return A data frame
#'   
#' @examples
#' \dontrun{
#' data <- combine_conc_conversions(
#'   concentration_std, db_concentration_unit_codes
#' )
#' }
combine_conc_conversions <- function(concentration_std, 
                                     db_concentration_unit_codes) {
  
  concentration_std <- concentration_std |>
    dplyr::mutate(
      code = stringr::str_squish(.data$code) 
    ) |>
    dplyr::select(
      "code", "conc_conversion_flag", "conc_conversion_value_multiplier",
      "conc_conversion_unit"
    )
  
  # print out name of any codes that don't match the db ones
  dont_match <- !(concentration_std$code %in% db_concentration_unit_codes$code)
  if (any((dont_match))) {
    message("Value(s) do not match code(s) in `concentration_unit_codes` table in ECOTOX database:")
    message(chk::cc(concentration_std$code[dont_match]))
  }
  
  # add conversion values to the concentration unit table
  concentration_unit_codes_std <- db_concentration_unit_codes |>
    dplyr::left_join(concentration_std, by = "code") |>
    tibble::tibble() 
  
  concentration_unit_codes_std
}

#' Read Concentration Unit Conversation Values
#' 
#' Internal to allow for testing
#'
#' @param concentration_std_file_path A file path
#' @param db_concentration_unit_codes A data frame
#'
#' @return A data frame
#'   
#' @examples
#' \dontrun{
#'  concentration_unit_codes_std <-  read_conc_conversions(
#'    concentration_std_file_path, db_concentration_unit_codes
#'  )
#' }
read_conc_conversions <- function(concentration_std_file_path, db_concentration_unit_codes) {
  concentration_std <- readr::read_csv(
    concentration_std_file_path, show_col_types = FALSE
  )
  
  chk::check_data(
    concentration_std, 
    list(
      code = c("", NA),
      conc_conversion_flag = c(TRUE, NA), 
      conc_conversion_value_multiplier = c(1, NA),
      conc_conversion_unit = c("", NA)
    )
  )
  
  data <- combine_conc_conversions(
    concentration_std, db_concentration_unit_codes
  )
  data 
}
