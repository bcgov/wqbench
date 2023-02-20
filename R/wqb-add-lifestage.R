#' Add corrected Life stage codes to Database
#' 
#' The corresponding life stage codes for fish and amphibians.
#'
#' @param database A string to the location of the database.
#' @return Invisible data frame
#' @return
#' @export
#'
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
      description_lifestage = "",
      simple_lifestage = ""
    )
  )
  
  lifestage_codes <- lifestage_codes |>
    dplyr::mutate(
      description_lifestage = stringr::str_squish(description_lifestage),
      simple_lifestage = stringr::str_squish(simple_lifestage)
    )
  
  # read in tables from db
  on.exit(DBI::dbDisconnect(con))
  con  <- DBI::dbConnect(
    RSQLite::SQLite(), 
    database
  )
  db_lifestage_codes <- DBI::dbReadTable(con, "lifestage_codes")
  
  
}