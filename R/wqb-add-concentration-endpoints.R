#' Add the Concentration Endpoints to Database
#'
#' Read in the selected concentration endpoints and mark those values in the
#' endpoint_code table in the database by adding a `concentration_flag` column
#' to the table.
#'
#' @param database A string to the location of the database.
#' @return Invisible data frame
#' @export
#' @details The list of concentration endpoints to be used is contained in a csv
#'   file in the extdata folder of the package. The csv file can be edited by
#'   adding or removing rows.  To add new rows get the `code` and `description`
#'   values from the `endpoint_code` table in the ECOTOX data and paste them
#'   into the csv file.
#'
#'   Do not add new columns, rename columns or rename the file. The file must
#'   only contain the `code` and `description` column.
#'
#'   The `code` values in the endpoint-concentration file are matched to the
#'   `code` values in the endpoint_code table in the ECOTOX downloaded data. Any
#'   codes that are listed in the file are then coded as TRUE under a new column
#'   called `concentration_flag` in the endpoint_table.
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
wqb_add_concentration_endpoints <- function(database) {
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
    print("Value(s) do not match code(s) in `endpoint_code` table in ECOTOX database:")
    print(endpoint_concentration_pick$code[dont_match])
  }
  
  endpoint_concentration <- db_endpoint_code |> 
    dplyr::left_join(endpoint_concentration_pick, by = "code") |>
    dplyr::mutate(
      concentration_flag =  tidyr::replace_na(.data$concentration_flag, FALSE)
    ) |>
    tibble::tibble()
  
  # write new table to database
  DBI::dbExecute(
    con,
    paste0(
      "CREATE TABLE endpoint_concentration (",
      "code TEXT, description TEXT, concentration_flag TEXT, ",
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
  
  DBI::dbExecute(con, "DROP TABLE endpoint_codes;")
  DBI::dbExecute(
    con,
    "ALTER TABLE endpoint_concentration
  RENAME TO endpoint_codes;"
  )
  
  invisible(endpoint_concentration)
}