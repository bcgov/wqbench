#' Add Habitat Information for each Species to Database
#'
#' Aggregate the sub habitat info from the tests table for each species.
#' Habitats are classified into three main groups: freshwater, brackish and
#' marine. Adds the resulting table into the database. 
#'
#' @param database A string to the location of the database.
#' @return Invisible data frame
#' @export
#' @details This function reads in the test table and aggregates the sub 
#'  habitat column data for each aquatic species. 
#'  
#'  The output table contains four columns: species number, freshwater, brackish 
#'  and marine. If the species was reported to be from a certain habitat the 
#'  value has been coded as TRUE. Species can be reported to be from more then 
#'  one habitat type. 
#' 
#'  The output table is added to the database with the name `species_habitat`.
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
  chk::chk_string(database)
}