#' Add Concentration Unit Conversation Values for Standardizing the Data
#'
#' Read in the concentration-conversion file and flag unit values that can be
#' converted and which ones will be removed. The column `conc_conversion_flag`
#' is added to the concentration_unit_codes table to indicate which units will
#' be kept and which will have the rows deleted. The
#' `conc_conversion_value_multiplier` column is added for each unit will be
#' converted to the unit specified in the `conc_conversion_unit` column.
#'
#' @param database A string to the location of the database.
#' @return Invisible data frame
#' @export
#' @details The list of units to be converted are contained in a csv file in the
#'   extdata folder of the package. The csv file can be edited by adding or
#'   removing rows.  To add new rows get the `code` and `description` values
#'   from the `concentration_unit_codes` table in the ECOTOX data and paste them
#'   into the csv file.
#'
#'   Do not add new columns, rename columns or rename the file. The file must
#'   only contain the columns: `code`, `description`, `conc_conversion_flag`,
#'   `conc_conversion_value_multiplier` and `conc_conversion_unit`.
#'
#'   The `code` values in the concentration-conversion file are matched to the
#'   `code` values in the `concentration_unit_codes` table in the ECOTOX
#'   downloaded data.
#'
#'   The `conc_conversion_value_multiplier` column contains the value need to
#'   convert the unit into the unit listed in the `conc_conversion_unit` column.
#' @examples
#' \dontrun{
#' concentration_unit_code_standardization <- wqb_standardize_concentration(
#'  database = "ecotox_ascii_09_15_2022.sqlite"
#' )
#'
#' concentration_unit_code_standardization <- wqb_standardize_concentration(
#'  database = "ecotox_db/ecotox_ascii_09_15_2022.sqlite"
#' )
#' }
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
  
  # read in lookup file 
  concentration_std_file_path <- system.file(
    "extdata/concentration-conversion.csv",
    package = "wqbench"
  )
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
    print("Value(s) do not match code(s) in `concentration_unit_codes` table in ECOTOX database:")
    print(concentration_std$code[dont_match])
  }
  
  # add conversion values to the concentration unit table
  concentration_unit_codes_std <- db_concentration_unit_codes |>
    dplyr::left_join(concentration_std, by = "code") |>
    tibble::tibble() 
  
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
  DBI::dbExecute(con, "DROP TABLE concentration_unit_codes;")
  DBI::dbExecute(
    con,
    "ALTER TABLE concentration_unit_codes_std
  RENAME TO concentration_unit_codes;"
  )
  
  invisible(concentration_unit_codes_std)
}
