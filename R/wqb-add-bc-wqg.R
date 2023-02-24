#' Add BC Water Quality Guidelines to Database
#'
#' Read in the British Columbia water quality guidelines (wqg) and add to the
#' chemicals tables in the database.
#'
#' @param database A string to the location of the database.
#' @return Invisible data frame
#' @export
#' @details The BC wag data is stored in the BC Data Catalogue.
#'
#'   The `CAS_number` column in the bc wqg data are matched to the `cas_number`
#'   column in the chemicals table of the ECOTOX downloaded data. A new column
#'   `present_in_bc_wqg` is added to the chemicals table that codes each
#'   chemical as either TRUE or FALSE.
#'
#' @examples
#' \dontrun{
#' chem_bc_wqg <- wqb_add_bc_wqg(
#'  database = "ecotox_ascii_09_15_2022.sqlite"
#' )
#'
#' chem_bc_wqg <- wqb_add_bc_wqg(
#'  database = "ecotox_db/ecotox_ascii_09_15_2022.sqlite"
#' )
#' }
wqb_add_bc_wqg <- function(database) {
  chk::chk_file(database)
  chk::chk_ext(database, "sqlite")
  
  # read in chemicals from db
  on.exit(DBI::dbDisconnect(con))
  con  <- DBI::dbConnect(
    RSQLite::SQLite(), 
    database
  )
  
  db_chemicals <- DBI::dbReadTable(con, "chemicals") |>
    dplyr::mutate(
      cas_number = as.character(.data$cas_number),
    ) |>
    tibble::tibble()
  
  if ("present_in_bc_wqg" %in% colnames(db_chemicals)) {
    stop(
      paste(
        "British Columbia water quality guideline flag has already been", 
        "added to the database"
      )
    )
  }
  # read in bc wqg 
  # # pull from BC data when uploaded
  # limits <-  bcdata::bcdc_get_data(
  #   record = "85d3990a-ec0a-4436-8ebd-150de3ba0747", 
  #   resource = "6f32a85b-a3d9-44c3-9a14-15175eba25b6"
  # )
  
  bc_wqg_file_path <- system.file(
    "extdata/all-wqgs.csv",
    package = "wqbench"
  )
  bc_wqg <- readr::read_csv(bc_wqg_file_path, show_col_types = FALSE) 
  chk::check_data(
    bc_wqg, 
    list(
      CAS_number = ""
    )
  )
  bc_wqg <- bc_wqg |>
    dplyr::select("CAS_number") |>
    dplyr::mutate(
      CAS_number = stringr::str_squish(.data$CAS_number),
      CAS_number = stringr::str_replace(.data$CAS_number, "^\\(", ""),
      CAS_number = stringr::str_replace(.data$CAS_number, "\\)$", ""),
      CAS_number = stringr::str_replace_all(.data$CAS_number, "\\-", ""),
      CAS_number = stringr::str_replace_all(.data$CAS_number, "[:alpha:]|[:space:]", ""),
      CAS_number = dplyr::na_if(.data$CAS_number, ""),
      present_in_bc_wqg = TRUE
    ) |> 
    tidyr::drop_na("CAS_number") |>
    dplyr::rename(cas_number = "CAS_number") |>
    dplyr::distinct()
  
  # add bc wqg flag to chemicals table
  chemicals_bc_wqg <- db_chemicals |>
    dplyr::left_join(bc_wqg, by = "cas_number") |>
    dplyr::mutate(
      cas_number = as.numeric(.data$cas_number),
      present_in_bc_wqg = tidyr::replace_na(.data$present_in_bc_wqg, FALSE)
    ) |>
    tibble::tibble() 
  
  # create db tables
  DBI::dbExecute(
    con,
    paste0(
      "CREATE TABLE chemicals_bc_wqg ",
      "(", paste(colnames(chemicals_bc_wqg), collapse = ", "),
      ", PRIMARY KEY (cas_number))"
    )
  )
  
  DBI::dbWriteTable(
    con,
    "chemicals_bc_wqg",
    value = chemicals_bc_wqg,
    append = TRUE,
    row.names = FALSE
  )
  
  DBI::dbExecute(con, "DROP TABLE chemicals;")
  DBI::dbExecute(
    con,
    "ALTER TABLE chemicals_bc_wqg
  RENAME TO chemicals;"
  )
  
  invisible(chemicals_bc_wqg)
}