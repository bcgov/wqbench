#' Filter Tests to Aquatic Species 
#'
#' Filter to only tests on aquatic species. 
#'
#' @param database A string to the location of the database.
#' @return Invisible data frame
#' @export
#' @details The tests table is filtered down to only aquatic tests which is when the
#'  `organism_habitat` column in the tests table is equal to "Water".
#'  
#'  The output table is added to the database with the name
#'   `tests_aquatic`.
#'
#' @examples
#' \dontrun{
#' wqb_filter_aquatic_tests(
#'   database = "ecotox_ascii_09_15_2022.sqlite"
#' )
#' 
#' wqb_filter_aquatic_tests(
#'   database = "ecotox_db/ecotox_ascii_09_15_2022.sqlite"
#' )
#' }
wqb_filter_aquatic_tests <- function(database) {
  chk::chk_file(database)
  chk::chk_ext(database, "sqlite")
  
  # read in results from db
  on.exit(DBI::dbDisconnect(con))
  con  <- DBI::dbConnect(
    RSQLite::SQLite(), 
    database
  )
  db_tests <- DBI::dbReadTable(con, "tests")
  
  # filter to only water/aquatic based tests 
  tests_aquatic <- db_tests |>
    dplyr::filter(.data$organism_habitat == "Water") |>
    tibble::tibble()
  
  # write new table to database
  DBI::dbExecute(
    con, 
    paste0(
      "CREATE TABLE tests_aquatic ", 
      "(", paste(colnames(tests_aquatic), collapse = ", "), 
      ", PRIMARY KEY (test_id))"
    )
  )
  DBI::dbWriteTable(
    con, 
    "tests_aquatic", 
    value = tests_aquatic, 
    append = TRUE, 
    row.names = FALSE
  )
  
  invisible(tests_aquatic)
}