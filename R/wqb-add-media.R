#' Add and Code Media Type to Salt or Fresh Water by Species and Test
#' 
#' The media type code for each species and test has been coded to either salt water, 
#' fresh water or not reported. 
#'
#' @param database A string to the location of the database.
#' @return Invisible data frame
#' @export
#' @details This function reads in the test table and codes each value to one 
#'   of three options: salt water, fresh water or not reported.
#' 
#'  The output table is added to the database with the name 
#'  `species_tests_media`.
#' @examples
#' \dontrun{
#' habitat_info <- wqb_add_habitat(
#'  database = "ecotox_ascii_09_15_2022.sqlite"
#' ) 
#' 
#' habitat_info <- wqb_add_habitat(
#'  database = "ecotox_db/ecotox_ascii_09_15_2022.sqlite"
#' ) 
#' }
wqb_add_habitat <- function(database) {
  chk::chk_file(database)
  chk::chk_ext(database, "sqlite")
  
  # read in tables from db
  on.exit(DBI::dbDisconnect(con))
  con  <- DBI::dbConnect(
    RSQLite::SQLite(), 
    database
  )
  db_tests <- DBI::dbReadTable(con, "tests")
  db_media_type <- DBI::dbReadTable(con, "media_type_codes")
  
  chk::check_data(
    db_tests, 
    list(
      species_number = 1L,
      media_type = ""
    )
  )
  
  species_tests_media <- db_tests |>
    dplyr::left_join(db_media_type, by = c("media_type" = "code")) |>
    dplyr::mutate(
      media_type_code = dplyr::case_when(
        .data[["description"]] == "Fresh water" ~ "fresh water",
        .data[["description"]] == "Salt water" ~ "salt water",
        TRUE ~ "not reported"
      )
    ) |>
    dplyr::select(
      "species_number", "test_id", "media_type_code"
    ) |>
    dplyr::arrange(.data[["species_number"]], .data[["test_id"]]) |>
    tibble::tibble()
  
  DBI::dbExecute(
    con,
    paste0(
      "CREATE TABLE species_tests_media ",
      "(species_number, test_id, media_type_code, ",
      "PRIMARY KEY (species_number, test_id))"
    )
  )
  
  DBI::dbWriteTable(
    con, 
    "species_tests_media", 
    value = species_tests_media, 
    append = TRUE, 
    row.names = FALSE
  )
  
  invisible(species_tests_media)
}