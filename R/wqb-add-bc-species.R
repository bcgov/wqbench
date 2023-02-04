#' Add BC Species to Database
#' 
#' Read in the list of BC species and then categorize each species as either 
#' present in BC or not. 
#'
#' @param file_path A string of the file location for the file that contains
#'  the list of BC Species. 
#' @param database A string to the location of the database.
#' @return Invisible data frame
#' @export
#' @details The BC species list must contain a single column called latin_name.
#' The latin_name column must consist of the genus and species separated by a 
#' space.
#' 
#' The output table contains two columns: species_number and bc_species 
#' 
#' The output table is added to the database with the name `species_bc_species`.
#' @examples
wqb_add_bc_species <- function(file_path, database) {
  chk::chk_string(file_path)
  chk::chk_string(database)
}