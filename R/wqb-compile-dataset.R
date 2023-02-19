#' Compile the data set from the Database 
#'
#' Compile data from the database to make the completed data set from the 
#' external sources and US EPA ECOTOX database.
#'
#' @param database A string to the location of the database.
#' @return Invisible data frame
#'
#' @return
#' @export
#'
#' @examples
wqb_compile_dataset <- function(database) {
  chk::chk_file(database)
  chk::chk_ext(database, "sqlite")
  
  on.exit(DBI::dbDisconnect(con))
  con  <- DBI::dbConnect(
    RSQLite::SQLite(), 
    database
  )
 
  db_species_british_columbia <- DBI::dbReadTable(con, "species_british_columbia")
  db_species_trophic_group <- DBI::dbReadTable(con, "species_trophic_group")
  db_species_tests_media  <- DBI::dbReadTable(con, "species_tests_media")
  db_tests_aquatic <- DBI::dbReadTable(con, "tests_aquatic")
  db_results_endpoint_concentration <- DBI::dbReadTable(con, "results_endpoint_concentration")
  
  db_references <- DBI::dbReadTable(con, "references")
  db_species <- DBI::dbReadTable(con, "species")
  db_chemicals <- DBI::dbReadTable(con, "chemicals")
  db_effect_codes <- DBI::dbReadTable(con, "effect_codes") 
  
  
}