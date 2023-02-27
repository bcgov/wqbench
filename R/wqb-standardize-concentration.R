#' Title
#'
#' @param database A string to the location of the database.
#' @return Invisible data frame
#' @export
#'
#' @examples
wqb_standardize_concentration <- function(database) {
  chk::chk_file(database)
  chk::chk_ext(database, "sqlite")
  
  # read in from db
  on.exit(DBI::dbDisconnect(con))
  con  <- DBI::dbConnect(
    RSQLite::SQLite(), 
    database
  )
  
  db_concentration_unit_codes <- DBI::dbReadTable(con, "concentration_unit_codes") |>
    dplyr::mutate(
      code = stringr::str_squish(.data$code),
    ) |>
    tibble::tibble()
  
  if ("concentration_value_multiplier" %in% colnames(db_concentration_unit_codes)) {
    stop(
      paste(
        "Concentration value multiplier has already been added to the database"
      )
    )
  }
  
  if ("concentration_units_to_keep" %in% colnames(db_concentration_unit_codes)) {
    stop(
      paste(
        "Concentration units to keep has already been added to the database"
      )
    )
  }
  
  # read in lookup file 
  concentration_std_file_path <- system.file(
    "extdata/concentration-conversion.csv",
    package = "wqbench"
  )
  concentration_std <- readr::read_csv(
    concentration_std_file_path, show_col_types = FALSE
  )
  
}