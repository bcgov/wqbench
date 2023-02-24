#' Add Life Stage groups to Database
#'
#' Read in the life stage simple groups and mark those values in the
#' lifestage_code table in the database by adding a `simple_lifestage` column to
#' the table.
#'
#' @param database A string to the location of the database.
#' @return Invisible data frame
#' @export
#' @details  Only life stages related to fish or amphibians have been coded. The
#'   purpose of the coding is to be able to simplify the many life stages into
#'   three categories: els (early life stage), juveniles and adults. Not all
#'   life stages have been coded into these three groups.
#'
#'   The life stage data is contained in a csv file in the extdata folder of the
#'   package. The csv file can be edited by adding or removing rows.  To add new
#'   rows get the `code` and `description` values from the `lifestage_code`
#'   table in the ECOTOX data and paste them into the csv file and then add the
#'   value to the `simple_lifestage` column.
#'
#'   Do not add new columns, rename columns or rename the file. The file must
#'   only contain the `code`, `description_lifestage` and `simple_lifestage`
#'   column.
#'
#'   The `code` values in the lifestage-codes.csv file are matched to the `code`
#'   values in the lifestage_code table in the ECOTOX downloaded data. Any codes
#'   that match are coded in a new column called `simple_lifestage`.
#' @examples
#' \dontrun{
#' lifestage_codes <- wqb_add_lifestage(
#'  database = "ecotox_ascii_09_15_2022.sqlite"
#' ) 
#' 
#' lifestage_codes <- wqb_add_lifestage(
#'  database = "ecotox_db/ecotox_ascii_09_15_2022.sqlite"
#' ) 
#' }
wqb_add_lifestage <- function(database) {
  chk::chk_file(database)
  chk::chk_ext(database, "sqlite")
  
  # read in table from db
  on.exit(DBI::dbDisconnect(con))
  con  <- DBI::dbConnect(
    RSQLite::SQLite(), 
    database
  )
  db_lifestage_codes <- DBI::dbReadTable(con, "lifestage_codes") |>
    dplyr::mutate(
      code = stringr::str_squish(.data$code)
    )
  if ("simple_lifestage" %in% colnames(db_lifestage_codes)) {
    stop(
      "Simple lifestage has already been added to the database"
    )
  }
  # read in life stage groups 
  lifestage_file_path <- system.file(
    "extdata/lifestage-codes.csv",
    package = "wqbench"
  )
  lifestage_codes <- readr::read_csv(
    lifestage_file_path,
    show_col_types = FALSE
  )
  chk::check_data(
    lifestage_codes,
    list(
      code = "",
      description_lifestage = "",
      simple_lifestage = ""
    )
  )
  lifestage_codes <- lifestage_codes |>
    dplyr::mutate(
      code = stringr::str_squish(.data$code),
      simple_lifestage = stringr::str_squish(.data$simple_lifestage),
      simple_lifestage = stringr::str_to_lower(.data$simple_lifestage)
    ) |>
    dplyr::select("code", "simple_lifestage")
  # print out name of any codes that don't match the db ones
  dont_match <- !(lifestage_codes$code %in% db_lifestage_codes$code)
  if (any((dont_match))) {
    print("Value(s) do not match code(s) in `endpoint_code` table in ECOTOX database:")
    print(lifestage_codes$code[dont_match])
  }
  
  lifestage_groups <- db_lifestage_codes |>
    dplyr::mutate(
      code = stringr::str_squish(.data$code)
    ) |>
    dplyr:: left_join(lifestage_codes, by = "code") |>
    tibble::tibble() 
  
  # write new table to database
  DBI::dbExecute(
    con,
    paste0(
      "CREATE TABLE lifestage_groups ",
      "(code TEXT, description TEXT, simple_lifestage TEXT, ",
      "PRIMARY KEY (code))"
    )
  )
  DBI::dbWriteTable(
    con, 
    "lifestage_groups", 
    value = lifestage_groups, 
    append = TRUE, 
    row.names = FALSE
  )
  DBI::dbExecute(con, "DROP TABLE lifestage_codes;")
  DBI::dbExecute(
    con,
    "ALTER TABLE lifestage_groups
  RENAME TO lifestage_codes;"
  )
  
  invisible(lifestage_groups)
}