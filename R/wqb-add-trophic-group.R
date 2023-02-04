#' Add Trophic Group to Species in the Database
#' 
#' Read in and add the trophic groups to the species. Species are classed into
#' groups and class level groups. 
#'
#' @param file_path A string to the file that contains the trophic information.  
#' @param database A string to the location of the database.
#' @return Invisible data frame
#' @export
#' @details The trophic groups file must contain the columns: class, order, 
#' ecological_group, ecological_group_class.
#' 
#' The output table contains three columns: species_number, ecological_group, 
#' and ecological_group_class.
#'
#' The output table is added to the database with the name 
#' `species_trophic_group`.
#' @examples
wqb_add_trophic_group <- function(file_path, database) {
  chk::chk_string(file_path)
  chk::chk_string(database)
}