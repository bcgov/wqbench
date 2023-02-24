#' Add Media Type Coding of Salt or Fresh Water
#'
#' Code the media type information into three categories: salt water, fresh
#' water and not reported.
#'
#' @param database A string to the location of the database.
#' @return Invisible data frame
#' @export
#' @details Read in the media_type_codes table and code the 24 groups into three
#'   categories: salt water, fresh water or not reported. The new categories are
#'   added to a column in the table called `media_type`.
#'
#' @examples
#' \dontrun{
#' media_info <- wqb_add_media(
#'  database = "ecotox_ascii_09_15_2022.sqlite"
#' )
#'
#' media_info <- wqb_add_media(
#'  database = "ecotox_db/ecotox_ascii_09_15_2022.sqlite"
#' )
#' }
wqb_add_media <- function(database) {
  chk::chk_file(database)
  chk::chk_ext(database, "sqlite")
  
  # read in tables from db
  on.exit(DBI::dbDisconnect(con))
  con  <- DBI::dbConnect(
    RSQLite::SQLite(),
    database
  )
  db_media_type <- DBI::dbReadTable(con, "media_type_codes") |>
    dplyr::mutate(
      description = stringr::str_squish(.data$description)
    )
  
  if ("media_type" %in% colnames(db_media_type)) {
    stop(
      "Media type has already been added to the database"
    )
  }
  # create coding groups
  media_type <- db_media_type |>
    dplyr::mutate(
      media_type = dplyr::case_when(
        .data[["description"]] == "Fresh water" ~ "fresh water",
        .data[["description"]] == "Salt water" ~ "salt water",
        TRUE ~ "not reported"
      )
    ) |>
    tibble::tibble()
  # write new tables 
  DBI::dbExecute(
    con,
    paste0(
      "CREATE TABLE media_type ",
      "(code TEXT, description TEXT, media_type TEXT, ",
      "PRIMARY KEY (code))"
    )
  )
  DBI::dbWriteTable(
    con,
    "media_type",
    value = media_type,
    append = TRUE,
    row.names = FALSE
  )
  DBI::dbExecute(con, "DROP TABLE media_type_codes;")
  DBI::dbExecute(
    con,
    "ALTER TABLE media_type
  RENAME TO media_type_codes;"
  )
  
  invisible(media_type)
}