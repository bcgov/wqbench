#' Add BC Species to Database
#'
#' Read in the British Columbia species and add to the `species` table in the
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
#'   `latin_name` column in the species table of the ECOTOX downloaded data. A
#'   new column `present_in_bc` is added to the species table that codes each
#'   species as either TRUE or FALSE.
#'
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
  chk::check_data(
    bc_species, 
    list(
      latin_name = ""
    )
  )
  bc_species <- bc_species |>
    dplyr::mutate(
      latin_name = stringr::str_squish(latin_name),
      present_in_bc = TRUE
    ) 
  # read in species from db
  on.exit(DBI::dbDisconnect(con))
  con  <- DBI::dbConnect(
    RSQLite::SQLite(), 
    database
  )
  db_species <- DBI::dbReadTable(con, "species")
  
  # combine and filter to only bc species 
  species_british_columbia <- db_species |>
    dplyr::mutate(latin_name = stringr::str_squish(latin_name)) |> 
    dplyr::left_join(bc_species, by = "latin_name") |>
    dplyr::mutate(present_in_bc =  tidyr::replace_na(present_in_bc, FALSE)) |>
    tibble::tibble()
  
  DBI::dbExecute(
    con, 
    paste0(
      "CREATE TABLE species_british_columbia ", 
      "(", paste(colnames(species_british_columbia), collapse = ", "), 
      ", PRIMARY KEY (species_number))"
    )
  )
  
  DBI::dbWriteTable(
    con, 
    "species_british_columbia", 
    value = species_british_columbia, 
    append = TRUE, 
    row.names = FALSE
  )
  
  DBI::dbExecute(con, "DROP TABLE species;")
  DBI::dbExecute(
    con,
    "ALTER TABLE species_british_columbia
  RENAME TO species;"
  )
  
  invisible(species_british_columbia)
}
