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

#' Create the Data Set for the App
#' 
#' This function downloads the data, creates and adds all the data to the
#' database, compiles, standardizes, and classifies the data. The output
#' of this function is feed into the app. 
#'
#' @param file_path A string of the file path location to save the downloaded
#'   files. The default is your current working directory.
#' @param version An integer to indicate which version you want to download. The
#'   default is 1 which downloads the most recent version.
#' @param folder_path Folder path to write to.  
#' @param quiet Turn off message when quiet set to TRUE.
#' @param ask Turn off question when set to FALSE.
#' @return A data frame
#' @export
#'
#' @examples
#' \dontrun{
#'  wqb_create_data_set(file_path = "~/Ecotoxicology/ecotox", 
#'  version = 1, 
#'  folder_path = "~/Ecotoxicology/ecotox_db/")
#' }
wqb_create_data_set <- function(file_path = "~/Ecotoxicology/ecotox", 
                                version = 1, 
                                folder_path = "~/Ecotoxicology/ecotox_db/",
                                quiet = FALSE,
                                ask = TRUE) {

  data_path <- wqb_download_epa_ecotox(
    file_path = file_path, version = version, ask = ask, quiet = quiet
  )
  
  database <- wqb_create_epa_ecotox(
    folder_path = folder_path,
    data_path = data_path,
    quiet = quiet,
    ask = ask
  )
  
  wqb_add_bc_species(database = database, quiet = quiet) 
  wqb_add_bc_wqg(database = database, quiet = quiet)
  wqb_add_concentration_endpoints(database = database, quiet = quiet)
  wqb_add_lifestage(database = database, quiet = quiet) 
  wqb_add_media(database = database, quiet = quiet)
  wqb_add_trophic_group(database = database, quiet = quiet) 
  wqb_add_duration_conversions(database = database, quiet = quiet)
  wqb_add_conc_conversions(database = database, quiet = quiet)
  
  data <- wqb_compile_dataset(database = database, quiet = quiet) 
  data <- wqb_classify_duration(data, quiet = quiet)
  data <- wqb_standardize_effect(data, quiet = quiet)
  data
}