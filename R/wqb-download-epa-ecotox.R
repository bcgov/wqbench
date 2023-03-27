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

#' Download ECOTOX Data Files
#'
#' Download the ECOTOX data files from their FTP website. The default is to
#' download the most recent version of the data.
#'
#' @param file_path A string of the file path location to save the downloaded
#'   files. The default is your current working directory.
#' @param version An integer to indicate which version you want to download. The
#'   default is 1 which downloads the most recent version.
#' @param quiet Turn off message when quiet set to TRUE.
#' @param ask Turn off question when set to FALSE.
#' @return Invisible string of the file path the downloaded files were saved.
#' @export
#' @details You have the option of downloading older version of the data but
#'   only up to the four most recent version. The most recent version is set as
#'   1 and the oldest version is version 4.
#'
#'   The downloaded folder will contain various files that are needed to build
#'   the database.
#'
#'   You must have a working internet connection to run this function
#'   successfully.
#'
#' @references US EPA ECOTOX website: <https://cfpub.epa.gov/ecotox/>
#'
#'   Olker, J. H., Elonen, C. M., Pilli, A., Anderson, A., Kinziger, B.,
#'   Erickson, S., Skopinski, M., Pomplun, A., LaLone, C. A., Russom, C. L., &
#'   Hoff, D. (2022). The ECOTOXicology Knowledgebase: A Curated Database of
#'   Ecologically Relevant Toxicity Tests to Support Environmental Research and
#'   Risk Assessment. Environmental Toxicology and Chemistry, 41(6):1520-1539.
#'   https://doi.org/10.1002/etc.5324
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
wqb_download_epa_ecotox <- function(file_path = ".", version = 1, ask = TRUE,
                                    quiet = FALSE) {
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
  
  if (ask){
    dir_present <- dir.exists(file.path(file_path, file_name))
    if (dir_present) {
      # Ask to end or to continue and overwrite 
      answer <- utils::askYesNo(
        paste("The", file_name, "folder is already downloaded to", file_path,". Overwrite it?")
      )
      # if no or cancel then exit function
      if (!answer | is.na(answer)) {
        if (!quiet) {
          message("Skip downloading data files")
        }
        return(file.path(file_path, file_name))
      }
    }
  }

  temp_zip <- file.path(
    withr::local_tempdir(),
    file_info$file[version]
  )
  
  if (!quiet) {
    message("Downloading...")
  }
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
  
  # write meta file
  fileConn <- file(file.path(output_folder, "download_info.txt"))
  text_msg <- paste0(
    "download_date | version \n",
    as.character(Sys.time()), "| ",
    file_name
  )
  writeLines(text_msg, fileConn)
  close(fileConn)
  
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
      recursive = TRUE,
      showWarnings = FALSE
    )
    file.copy(
      to = files_to_move,
      from = files_from,
      overwrite = TRUE
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
      recursive = TRUE,
      showWarnings = FALSE
    )
    file.copy(
      to = files_to_move,
      from = files_from,
      overwrite = TRUE
    )
  }
  
  move_location
}
