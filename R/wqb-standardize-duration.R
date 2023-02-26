#' Add Duration Unit Conversation Values for Standardizing the Data
#'
#' Read in the duration-conversion file and flag unit values that can be
#' converted and which ones will be removed. The column `keep` is added to the
#' duration_unit_codes table to indicate which units will be kept and which will
#' have the rows deleted. The `value_multiplier_to_hours` column is added for
#' each unit will be converted to hours.
#'
#' @param database A string to the location of the database.
#' @return Invisible data frame
#' @export
#' @details The list of units to be converted are contained in a csv file in the
#'   extdata folder of the package. The csv file can be edited by adding or
#'   removing rows.  To add new rows get the `code` and `description` values
#'   from the `duration_unit_codes` table in the ECOTOX data and paste them into
#'   the csv file.
#'
#'   Do not add new columns, rename columns or rename the file. The file must
#'   only contain the columns: `code`, `description`, `keep` and
#'   `value_multiplier_to_hours`.
#'
#'   The `code` values in the duration-conversion file are matched to the `code`
#'   values in the `duration_unit_codes` table in the ECOTOX downloaded data.
#'
#'   The `value_multiplier_to_hours` column contains the value need to convert
#'   the unit into hours.
#' @examples
#' \dontrun{
#' duration_unit_code_standardization <- wqb_standardize_duration(
#'  database = "ecotox_ascii_09_15_2022.sqlite"
#' )
#'
#' duration_unit_code_standardization <- wqb_standardize_duration(
#'  database = "ecotox_db/ecotox_ascii_09_15_2022.sqlite"
#' )
#' }
wqb_standardize_duration <- function(database) {
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
  
  if ("value_multiplier_to_hours" %in% colnames(db_duration_unit_codes)) {
    stop(
      paste(
        "Value multiplier to hours has already been", 
        "added to the database"
      )
    )
  }
  
  if ("keep" %in% colnames(db_duration_unit_codes)) {
    stop(
      paste(
        "Keep has already been added to the database"
      )
    )
  }
  
  duration_std_file_path <- system.file(
    "extdata/duration-conversion.csv",
    package = "wqbench"
  )
  duration_std <- readr::read_csv(duration_std_file_path, show_col_types = FALSE) 
  chk::check_data(
    duration_std, 
    list(
      code = c("", NA),
      keep = TRUE,
      value__to_hours = c(1, NA)
    )
  )
  
  duration_std <- duration_std |>
    dplyr::mutate(
      code = dplyr::if_else(
        .data$description == "Pretreatment, time unknown" & .data$code == "#NAME?",
        "-X",
        .data$code
      ),
      code = stringr::str_squish(.data$code) 
    ) |>
    dplyr::select("code", "keep", "value_multiplier_to_hours")
  
  # print out name of any codes that don't match the db ones
  dont_match <- !(duration_std$code %in% db_duration_unit_codes$code)
  if (any((dont_match))) {
    print("Value(s) do not match code(s) in `duration_unit_codes` table in ECOTOX database:")
    print(duration_std$code[dont_match])
  }
  
  # add conversion values to the duration table
  duration_unit_codes_std <- db_duration_unit_codes |>
    dplyr::left_join(duration_std, by = "code") |>
    tibble::tibble() 
  
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
  
  DBI::dbExecute(con, "DROP TABLE duration_unit_codes;")
  DBI::dbExecute(
    con,
    "ALTER TABLE duration_unit_codes_std
  RENAME TO duration_unit_codes;"
  )
  
  invisible(duration_unit_codes_std)
}
