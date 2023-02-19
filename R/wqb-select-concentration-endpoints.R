#' Filter Results to Concentration Endpoint
#'
#' Determine the result ID's from concentration based endpoint tests. Logs the
#'  concentration endpoints that are in the log format. 
#'
#' @param database A string to the location of the database.
#' @return Invisible data frame
#' @export
#' @details The concentration endpoints that have been selected are contained in
#'   the extdata folder of the package in a file named
#'   `concentration-endpoints.csv`. This file can be edited by adding or deleting rows. 
#'   Do not add new columns, rename columns or rename the file.  The
#'   file must contain the columns named `code` and `description`.
#'   
#'   The full list of all possible endpoints is contained in the `endpoint_codes` table
#'   in the ECOTOX downloaded data. 
#'   
#'   The output table that is written to the database is the results table
#'   filtered to only contain the selected endpoints as listed in the 
#'   `concentration-endpoints.csv`.
#'
#'   The output table is added to the database with the name
#'   `results_endpoint_concentration`.
#' 
#' @examples
#' \dontrun{
#' wqb_select_concentration_endpoints(
#'   database = "ecotox_ascii_09_15_2022.sqlite"
#' )
#' 
#' wqb_select_concentration_endpoints(
#'   database = "ecotox_db/ecotox_ascii_09_15_2022.sqlite"
#' )
#' }
wqb_select_concentration_endpoints  <- function(database) {
  chk::chk_file(database)
  chk::chk_ext(database, "sqlite")
  
  # read in bc species 
  conc_endpoints_file_path <- system.file(
    "extdata/concentration-endpoints.csv",
    package = "wqbench"
  )
  conc_endpoints <- readr::read_csv(
    conc_endpoints_file_path, 
    show_col_types = FALSE
  ) 
  chk::check_data(conc_endpoints, list(code = ""))
  conc_endpoints <- conc_endpoints |>
    dplyr::mutate(
      code = stringr::str_squish(code)
    )
  # read in results from db
  on.exit(DBI::dbDisconnect(con))
  con  <- DBI::dbConnect(
    RSQLite::SQLite(), 
    database
  )
  db_results <- DBI::dbReadTable(con, "results")
  
  # filter to selected endpoints
  results_endpoint_concentration <- db_results |> 
    dplyr::mutate(
      endpoint = stringr::str_squish(endpoint)
    ) |>
    dplyr::filter(.data[["endpoint"]] %in% conc_endpoints$code) |>
    dplyr::mutate(
      ### need to deal with logged endpoints
      ### use 10^x as this is a logged concentration
    ) |>
    tibble::tibble()
  
  # write new table to database
  DBI::dbExecute(
    con, 
    paste0(
      "CREATE TABLE results_endpoint_concentration ", 
      "(", paste(colnames(results_endpoint_concentration), collapse = ", "), 
      ", PRIMARY KEY (result_id))"
    )
  )
  DBI::dbWriteTable(
    con, 
    "results_endpoint_concentration", 
    value = results_endpoint_concentration, 
    append = TRUE, 
    row.names = FALSE
  )
  
  invisible(results_endpoint_concentration)
}
