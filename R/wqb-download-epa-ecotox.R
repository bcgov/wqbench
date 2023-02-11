#' Download EPA ECOTOX Database Files
#'
#' Download the EPA ECOTOX database files from their FTP site. The default is to
#' download the most recent version of the database.
#'
#' @param file_path A string of the file path location to save the downloaded
#'   files. The default is your current working directory. 
#' @param version An integer to indicate which version you want to download.
#'   The default is 1 which downloads the most recent version.
#' @return Invisible string of the file path the downloaded files were saved.
#' @export
#' @details You have the option of downloading older version of the database 
#'  but only up to the four most recent version. The most recent version is set
#'  as 1 and the older version you can download is version 4. 
#'  
#'  The downloaded folder will contain various files that are needed to compile 
#'  the database. 
#' 
#' @examples
#' \dontrun{
#' wqb_download_epa_ecotox()
#' 
#' wqb_download_epa_ecotox("data_download")
#' 
#' # pull previous version of the database
#' wqb_download_epa_ecotox("data_download", version = 2)
#' }
wqb_download_epa_ecotox <- function(file_path = ".", version = 1) {
  chk::chk_string(file_path)
  chk::chk_whole_number(version)
  chk::chk_range(version, range = c(1, 4))
  
  ftp_url <- "https://gaftp.epa.gov/ecotox/"
  ftp_data <- ftp_url |>
    rvest::read_html() |>
    rvest::html_nodes("a") |>
    rvest::html_attr("href")
  closeAllConnections()
  zip_files <- grep("\\.zip$", ftp_data, value = TRUE)
  file_info <- data.frame(file = zip_files)
  
  file_info$dates <- as.Date(
    stringr::str_extract(
      zip_files, 
      "[0-9]{2}_[0-9]{2}_[0-9]{4}"
    ), 
    format = "%m_%d_%Y"
  )
  file_info$file_name <- tools::file_path_sans_ext(zip_files)
  
  file_info <- file_info[order(file_info$date, decreasing = TRUE),] 
  most_recent_version <- file_info$file[version] 
  file_url <- file.path(ftp_url, most_recent_version)
  file_name <-  file_info$file_name[version] 
  
  temp_zip <- file.path(
    tempdir(),
    file_info$file[version]
  )
  
  message("Downloading...")
  httr::GET(
    url = file_url,
    httr::write_disk(temp_zip, overwrite = TRUE),
    httr::progress("down")
  )
  
  unzip_location_sub <- file.path(tempdir(), "unzip")

  utils::unzip(
    zipfile = temp_zip,
    exdir = unzip_location_sub
  )
  
  output_folder <- unzip_folder_correction(
    unzip_location_sub, 
    file_name,
    file_path
  )
  
  invisible(output_folder)
}
