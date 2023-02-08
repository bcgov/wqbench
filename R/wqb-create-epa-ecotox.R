#' Create a SQLite Database from the US EPA ECOTOX data
#'
#'  Use this function to create a local SQLite database of the ECOTOX data.
#'
#' @param file_path A string of the file path location to save the SQLite 
#'  database
#' @param data_path A string of the location the downloaded ECOTOX data folder
#'
#' @return Invisible string of the file path of the database.
#' @export
#'
#' @examples
#' \dontrun{
#' wqb_create_epa_ecotox(data_path = "ecotox_ascii_12_15_2022")
#' }
wqb_create_epa_ecotox <- function(file_path = ".", data_path) {
  chk::chk_string(file_path)
  chk::chk_string(data_path)
  
  dbname <- paste0(basename(source), ".sqlite")
  dbfile <- file_path(file_path, dbname)
  con  <- DBI::dbConnect(
    RSQLite::SQLite(), 
    dbfile
  )
  
  
  files <- list.files(
    data_path,
    pattern = "*.txt", 
    full.names = TRUE
  )
  
  files_data <- files[!grepl('release', files)]
  name_data <- gsub(".txt", "", basename(files_data))
  
  DBI::dbDisconnect(conn)
  
  invisible(dbfile)
}

### Inputs
# - location of downloaded files
# -  SQLite or Postgres - need to down for other inputs (I think SQLite as local file cause why not)
#    - if SQLite then just need a second location for where the db will be made

### Outputs
# - if sqlite then path of output file 