#' Add BC Species to Database
#' 
#' Read in the list of BC species and add the list the SQLite database. 
#'
#' @param file_path A string of the file location for the csv file that contains
#'  the list of BC Species. 
#' @param database A string to the location of the database.
#' @return Invisible data frame
#' @export
#' @details The BC species list must contain a single column called latin_name.
#' The latin_name column must consist of the genus and species separated by a 
#' space.
#' 
#' The output table that is written to the database contains two columns: 
#' species_number and bc_species 
#' 
#' The output table is added to the database with the name 
#' `species_british_columbia`.
#' @examples
#' \dontrun{
#' # all files in root directory
#' bc_species <- wqb_add_bc_species(
#'  file_path = "bc-species.csv",
#'  database = "ecotox_ascii_09_15_2022.sqlite"
#' ) 
#' 
#' # files in subdirectories 
#' bc_species <- wqb_add_bc_species(
#'  file_path = "lookups/bc-species.csv",
#'  database = "ecotox_db/ecotox_ascii_09_15_2022.sqlite"
#' ) 
#' }
wqb_add_bc_species <- function(file_path, database) {
  chk::chk_file(file_path)
  chk::chk_ext(file_path, "csv")
  chk::chk_file(database)
  chk::chk_ext(database, "sqlite")
  
  # read in bc species 
  bc_species <- readr::read_csv(file_path, show_col_types = FALSE) 
  chk::check_data(bc_species, list(latin_name = ""))
  bc_species$present_in_bc <- TRUE
  
  # read in species from db
  on.exit(DBI::dbDisconnect(con))
  con  <- DBI::dbConnect(
    RSQLite::SQLite(), 
    database
  )
  db_species <- DBI::dbReadTable(con, "species")
  
  # combine and filter to only bc species 
  ecotox_bc_species <- db_species |>
    dplyr::left_join(bc_species, by = "latin_name") |>
    dplyr::select("species_number", "present_in_bc") |>
    tidyr::drop_na("present_in_bc") |>
    tibble::tibble()
  
  DBI::dbExecute(
    con,
    paste0("CREATE TABLE species_british_columbia ",
           "(species_number, present_in_bc, PRIMARY KEY (species_number))"
    )
  )
  
  DBI::dbWriteTable(
    con, 
    "species_british_columbia", 
    value = ecotox_bc_species, 
    append = TRUE, 
    row.names = FALSE
  )
  
  invisible(ecotox_bc_species)
}
