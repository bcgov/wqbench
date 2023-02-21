#' Add corrected Life stage codes to Database
#'
#' Code the ECOTOX life stage code values into three simple life stages: ELS
#' (early life stage), juveniles and adults.
#'
#' @param database A string to the location of the database.
#' @return Invisible data frame
#' @export
#' @details  Only life stages related to fish or amphibians have been coded. The
#'   purpose of the coding is to be able to simplify the many life stages into
#'   three main categories.
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
#'   The output table that is written to the database contains two columns:
#'   `code` and `simple_lifestage`
#'
#'   The output table is added to the database with the name `lifestage_groups`.
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
  
  # read in trophic groups 
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
    )
  
  # read in tables from db
  on.exit(DBI::dbDisconnect(con))
  con  <- DBI::dbConnect(
    RSQLite::SQLite(), 
    database
  )
  db_lifestage_codes <- DBI::dbReadTable(con, "lifestage_codes")
  
  
  lifestage_groups <- db_lifestage_codes |>
    dplyr::mutate(
      code = stringr::str_squish(.data$code)
    ) |>
    dplyr:: left_join(lifestage_codes, by = "code") |>
    dplyr::select("code", "simple_lifestage") |>
    tibble::tibble() |>
    tidyr::drop_na("simple_lifestage") 
  
  DBI::dbExecute(
    con,
    paste0("CREATE TABLE lifestage_groups ",
           "(code TEXT, simple_lifestage TEXT, PRIMARY KEY (code))"
    )
  )
  
  DBI::dbWriteTable(
    con, 
    "lifestage_groups", 
    value = lifestage_groups, 
    append = TRUE, 
    row.names = FALSE
  )
  
  invisible(lifestage_groups)
}