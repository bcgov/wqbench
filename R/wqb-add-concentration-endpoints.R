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

#' Add the Concentration Endpoints
#'
#' Read in the concentration endpoints that have been selected to keep in the
#' final data set. Add a column to the endpoint_codes table in the database to
#' mark the corresponding endpoints.
#'
#' @param database A string to the location of the database.
#' @param quiet Turn off message when quiet set to TRUE.
#' @return Invisible data frame
#' @export
#' @details The list of concentration endpoints is contained in a csv file in
#'   the extdata folder of the package. The csv file can be edited by adding or
#'   removing rows.  To add new rows get the `code` and `description` values
#'   from the `endpoint_codes` table in the database and paste them into the csv
#'   file.
#'
#'   Do not add new columns, rename columns or rename the file. The file must
#'   only contain the `code` and `description` column.
#'
#'   The `code` values in the endpoint-concentration file are matched to the
#'   `code` values in the endpoint_code table in the database. A new column
#'   `concentration_flag` is added to the endpoint_code table that codes each
#'   endpoint as TRUE if the endpoint is present in endpoint-concentration.csv
#'   file.
#' 
#' @examples
#' \dontrun{
#' wqb_add_concentration_endpoints(
#'   database = "ecotox_ascii_09_15_2022.sqlite"
#' )
#' 
#' wqb_add_concentration_endpoints(
#'   database = "ecotox_db/ecotox_ascii_09_15_2022.sqlite"
#' )
#' }
wqb_add_concentration_endpoints <- function(database, quiet = FALSE) {
  chk::chk_file(database)
  chk::chk_ext(database, "sqlite")
  
  # read in data from db
  on.exit(DBI::dbDisconnect(con))
  con  <- DBI::dbConnect(
    RSQLite::SQLite(), 
    database
  )
  db_endpoint_code <- DBI::dbReadTable(con, "endpoint_codes") |>
    dplyr::mutate(
      code = stringr::str_squish(.data$code)
    )
  
  if ("concentration_flag" %in% colnames(db_endpoint_code)) {
    stop(
      "Concentration endpoint flag has already been added to the database"
    )
  }
  
  # read in bc species 
  conc_endpoints_file_path <- system.file(
    "extdata/concentration-endpoints.csv",
    package = "wqbench"
  )
  endpoint_concentration_pick <- readr::read_csv(
    conc_endpoints_file_path, 
    show_col_types = FALSE
  ) 
  chk::check_data(endpoint_concentration_pick, list(code = ""))
  
  # add concentration endpoints to endpoints table
  endpoint_concentration_pick <- endpoint_concentration_pick |>
    dplyr::mutate(
      code = stringr::str_squish(.data$code),
      concentration_flag = TRUE
    ) |>
    dplyr::select("code", "concentration_flag")
  
  # print out name of any codes that don't match the db ones
  dont_match <- !(endpoint_concentration_pick$code %in% db_endpoint_code$code)
  if (any((dont_match))) {
    print("Value(s) do not match code(s) in `endpoint_codes` table in ECOTOX database:")
    print(endpoint_concentration_pick$code[dont_match])
  }
  
  endpoint_concentration <- db_endpoint_code |> 
    dplyr::left_join(endpoint_concentration_pick, by = "code") |>
    dplyr::mutate(
      concentration_flag =  tidyr::replace_na(.data$concentration_flag, FALSE)
    ) |>
    tibble::tibble()
  
  print("here1")
  # write new table to database
  DBI::dbExecute(
    con,
    paste0(
      "CREATE TABLE endpoint_concentration ",
      "(", paste(colnames(endpoint_concentration), collapse = ", "), 
      "PRIMARY KEY (code))"
    )
  )
  DBI::dbWriteTable(
    con, 
    "endpoint_concentration", 
    value = endpoint_concentration, 
    append = TRUE, 
    row.names = FALSE
  )
  if (!quiet) {
    message("Adding concentration endpoint list")
  }
  DBI::dbExecute(con, "DROP TABLE endpoint_codes;")
  DBI::dbExecute(
    con,
    "ALTER TABLE endpoint_concentration
  RENAME TO endpoint_codes;"
  )
  
  invisible(endpoint_concentration)
}