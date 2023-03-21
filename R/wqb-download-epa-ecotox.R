# Copyright 2023 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at 
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

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
    withr::local_tempdir(),
    file_info$file[version]
  )
  
  message("Downloading...")
  httr::GET(
    url = file_url,
    httr::write_disk(temp_zip, overwrite = TRUE),
    httr::progress("down")
  )
  
  unzip_location_sub <- file.path(withr::local_tempdir(), "unzip")

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


unzip_folder_correction <- function(unzip_location_sub, file_name, file_path) {
  # ensure folders unzip into the expected folder structure 
  # some files behaved differently during the unzip process
  dirs <- list.dirs(unzip_location_sub, full.names = FALSE, recursive = FALSE)
  
  if (any(dirs == file_name)) {
    files_from <- list.files(
      file.path(unzip_location_sub, file_name), 
      recursive = TRUE,
      full.names = TRUE
    )
    files_to_move <- file.path(
      file_path, 
      list.files(unzip_location_sub, recursive = TRUE)
    )
    move_location <- file.path(file_path, file_name)
    dir.create(
      file.path(move_location, "validation"), 
      recursive = TRUE
    )
    file.copy(
      to = files_to_move,
      from = files_from
    )
  } else {
    files_from <- list.files(
      file.path(unzip_location_sub), 
      recursive = TRUE,
      full.names = TRUE
    )
    move_location <- file.path(file_path, file_name)
    files_to_move <- file.path(
      move_location, 
      list.files(unzip_location_sub, recursive = TRUE)
    )
    dir.create(
      file.path(move_location, "validation"),
      recursive = TRUE
    )
    file.copy(
      to = files_to_move,
      from = files_from
    )
  }
  
  move_location
}
