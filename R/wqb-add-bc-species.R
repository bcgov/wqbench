#' Add BC Species to Database
#'
#' Read in the British Columbia species values and add them to the SQLite
#' database.
#'
#' @param database A string to the location of the database.
#' @return Invisible data frame
#' @export
#' @details The BC species data is contained in a csv file in the extdata folder
#'   of the package. This file can be edited by adding new species or removing
#'   species. Do not add new columns, rename columns or rename the file. The
#'   file must only contain a single column named `latin_name`.
#'
#'   The `latin_name` column must consist of the genus and species separated by
#'   a space. The `latin_name` column in the bc-species file are matched to the
#'   `latin_name` column in the species table of the ECOTOX downloaded data.
#'
#'   The output table that is written to the database contains two columns:
#'   species_number and bc_species
#'
#'   The output table is added to the database with the name
#'   `species_british_columbia`.
#' @examples
#' \dontrun{
#' bc_species <- wqb_add_bc_species(
#'  database = "ecotox_ascii_09_15_2022.sqlite"
#' ) 
#' 
#' bc_species <- wqb_add_bc_species(
#'  database = "ecotox_db/ecotox_ascii_09_15_2022.sqlite"
#' ) 
#' }
wqb_add_bc_species <- function(database) {
  chk::chk_file(database)
  chk::chk_ext(database, "sqlite")
  
  # read in bc species 
  bc_species_file_path <- system.file(
    "extdata/bc-species.csv",
    package = "wqbench"
  )
  bc_species <- readr::read_csv(bc_species_file_path, show_col_types = FALSE) 
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
