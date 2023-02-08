#' Download EPA ECOTOX Database Files
#'
#' Download the EPA ECOTOX database files from their FTP site. The default is to
#' download the most recent version of the database.
#'
#' @param file_path A string of the file path location to save the downloaded
#'   files. The default is your current working directory. 
#' @param version An integer to indicate which version you want to download.
#'   The default is 1 which downloads the most recent version.
#' @return A string of the file location
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
#' wqb_download_epa_ecotox("data")
#' }
wqb_download_epa_ecotox <- function(file_path = ".", version = 1) {
  chk::chk_string(file_path)
  chk::chk_whole_number(version)
  chk::chk_range(version, range = c(1, 4))
  
  ### getting timeout error... try tomorrow
  # ftp_url <- "ftp://newftp.epa.gov/ECOTOX/"
  # ftp_html_body <- ftp_url %>%
  #   rvest::read_html() %>% 
  #   rvest::html_text() %>% 
  #   strsplit(split = "\\s+") %>% 
  #   unlist()
  ### use other method for now that just pulls only the most recent from homepage
  ftp_url <- "https://cfpub.epa.gov/ecotox/index.cfm" |>
    rvest::read_html() |>
    rvest::html_elements("a.ascii-link") |>
    rvest::html_attr("href")
  closeAllConnections()
  zip_files <- grep("\\.zip$", ftp_url, value = TRUE)
  file_info <- data.frame(file = zip_files)
  
  file_info$dates <- as.Date(
    stringr::str_extract(
      zip_files, 
      "[0-9]{2}_[0-9]{2}_[0-9]{4}"
    ), 
    format = "%m_%d_%Y"
  )
  file_info$name <- stringr::str_extract(zip_files, "ecotox_.*")
  
  file_info <- file_info[order(file_info$date, decreasing = TRUE),] 
  most_recent_version <- file_info$file[version] 
  ### change pending other download ability
  #file_url <- file.path(ftp_url, most_recent_version)
  
  local_download_path <- file.path(
    file_path, 
    file_info$name[version]
  )
  
  temp_zip <- file.path(
    tempdir(),
    file_info$name[version]
  )
    
  message("File about to download...")
  httr::GET(
    url = file_info$file[version],
    httr::write_disk(temp_zip, overwrite = TRUE),
    httr::progress("down")
  )
  
  message("Unzipping file...")
  utils::unzip(
    zipfile = temp_zip,
    exdir = file_path
  )
  
  local_download_path
}

### working for now with method that only allows the most recent pull

