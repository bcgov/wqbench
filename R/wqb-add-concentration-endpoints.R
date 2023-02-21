#' Add the Concentration Endpoint List to Database
#' 
#' Add the list of selected concentration endpoints into the Database. 
#'
#' @param database A string to the location of the database.
#' @return Invisible data frame
#' @export
#' @details The list of concentration endpoints to be used is contained in a csv 
#'   file in the extdata folder of the package. The csv file can be edited by 
#'   adding or removing rows.  To add new
#'   rows get the `code` and `description` values from the `endpoint_code`
#'   table in the ECOTOX data and paste them into the csv file.
#'   
#'   Do not add new columns, rename columns or rename the file. The file must
#'   only contain the `code` and `description` column. 
#'
#'   The output table that is written to the database contains one columns:
#'   `code`.
#'
#'   The output table is added to the database with the name 
#'   `endpoint_concentration`.
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
wqb_add_concentration_endpoints <- function(database) {
  chk::chk_file(database)
  chk::chk_ext(database, "sqlite")
  
  # read in bc species 
  conc_endpoints_file_path <- system.file(
    "extdata/concentration-endpoints.csv",
    package = "wqbench"
  )
  endpoint_concentration <- readr::read_csv(
    conc_endpoints_file_path, 
    show_col_types = FALSE
  ) 
  chk::check_data(endpoint_concentration, list(code = ""))
  
  endpoint_concentration <- endpoint_concentration |>
    dplyr::mutate(
      code = stringr::str_squish(.data$code)
    ) |>
    dplyr::select("code")
  
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
  
  # print out name of any codes that don't match the db ones
  matches <- !(endpoint_concentration$code %in% db_endpoint_code$code)
  if (any((matches))) {
    print("Value(s) do not match code(s) in `endpoint_code` table in ECOTOX database:")
    print(endpoint_concentration$code[matches])
  }
  
  # write new table to database
  DBI::dbExecute(
    con,
    paste0("CREATE TABLE endpoint_concentration ",
           "(code TEXT, PRIMARY KEY (code))"
    )
  )
  DBI::dbWriteTable(
    con, 
    "endpoint_concentration", 
    value = endpoint_concentration, 
    append = TRUE, 
    row.names = FALSE
  )
  
  invisible(endpoint_concentration)
}