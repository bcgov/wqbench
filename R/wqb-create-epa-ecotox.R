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

#' Create the ECOTOX Database
#'
#' Create a SQLite database from the US EPA ECOTOX downloaded files.
#'
#' @param folder_path Folder path to write to.  
#' @param data_path Folder path to the downloaded ECOTOX folder
#' @param quiet Turn off message when quiet set to TRUE.
#' @param ask Turn off question when set to FALSE.
#' @return Invisible string of the file path of the database.
#' @export
#' @details This functions reads in the text files in the folder and writes
#' them to the database.  
#' 
#' This function will overwrite a database if already present. 
#'
#' @examples
#' \dontrun{
#' wqb_create_epa_ecotox(data_path = "ecotox_ascii_12_15_2022")
#' }
wqb_create_epa_ecotox <- function(folder_path = ".", data_path, quiet = FALSE,
                                  ask = TRUE) {
  chk::chk_string(folder_path)
  chk::chk_string(data_path)
  
  dbname <- paste0(basename(data_path), ".sqlite")
  dbfile <- file.path(folder_path, dbname)
  
  if (ask){
    file_present <- file.exists(dbfile)
    if (file_present) {
      # Ask to end or to continue and overwrite 
      answer <- utils::askYesNo(
        paste("The", dbname, "database is already present in", folder_path,". Overwrite it?")
      )
      # if no or cancel then exit function
      if (!answer | is.na(answer)) {
        chk::err("Permission denied. Exiting")
      }
    }
  }
  unlink(dbfile)
  dir.create(folder_path, showWarnings = FALSE)
  
  if (!quiet) {
    message("Creating SQLite database")
  }
  
  on.exit(DBI::dbDisconnect(con))
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
  files_data <- files_data[!grepl('download', files_data)]
  name_data <- gsub(".txt", "", basename(files_data))
  tbl_data <- db_tbl_core_structure()
  
  for (i in seq_along(files_data)) {
    if (!quiet) {
      message("Adding: ecotox table ", name_data[i])
    }
    dt <- utils::read.table(
      files_data[i], header = TRUE, sep = '|', comment.char = '', quote = ''
    )
    query <- paste0(
      "CREATE TABLE ", names(tbl_data[i]), 
      "(", paste(colnames(dt), collapse = ", "), 
      ", PRIMARY KEY (", tbl_data[i], "))"
    )
    DBI::dbExecute(con, query)
    DBI::dbWriteTable(con, 
      names(tbl_data[i]), value = dt, append = TRUE, row.names = FALSE
    )
  }
  
  # Create validation tables
  files_validation <- list.files(
    file.path(data_path, "validation"), pattern = "*.txt", full.names = TRUE
  )
  name_validation <- gsub(".txt", "", basename(files_validation))
  validation_data <- db_tbl_validation_structure()

  for (i in seq_along(files_validation)) {
    if (!quiet) {
      message("Adding: ecotox table ", name_validation[i])
    }
    dt <- utils::read.table(
      files_validation[i],
      header = TRUE,
      sep = "|",
      comment.char = "",
      quote = ""
    )
    if (!is.null(validation_data[[i]])) {
      query <- paste0(
        "CREATE TABLE [", names(validation_data[i]), 
        "] (", paste(colnames(dt), collapse = ", "), 
        ", PRIMARY KEY (", validation_data[[i]], "))"
      )
      DBI::dbExecute(con, query)
    }
    DBI::dbWriteTable(con, 
      names(validation_data[i]), value = dt, append = TRUE, row.names = FALSE
    )
  }
   # add version info and downloaded day
  dt <- utils::read.table(
    file.path(data_path, "download_info.txt"),
    header = TRUE,
    sep = "|",
    comment.char = "",
    quote = ""
  )

  query <- paste0(
    "CREATE TABLE meta_data_dl",
    "(", paste(colnames(dt), collapse = ", "),
    ")"
  )
  DBI::dbExecute(con, query)
  DBI::dbWriteTable(con,
    "meta_data_dl", value = dt, append = TRUE, row.names = FALSE
  )

  invisible(dbfile)
}

db_tbl_core_structure <- function() {
  # names are table names, values are primary keys 
  list(
    "chemical_carriers" = "carrier_id",
    "dose_response_details" = "dose_resp_detail_id",
    "dose_response_links" = "dose_resp_link_id",
    "dose_responses" = "dose_resp_id",
    "doses" = "dose_id",
    "media_characteristics" = "result_id",
    "results" = "result_id",
    "tests" = "test_id"
  )
}

### there are about 40 reference/validation tables but not all being pk'ed
db_tbl_validation_structure <- function() {
  # names are table names, values are primary keys 
  list(
    "application_frequency_codes" = "code",
    "application_type_codes" = "code",
    "chemical_analysis_codes" = "code",
    "chemical_formulation_codes" = NULL,
    "chemical_grade_codes" = NULL,
    "chemicals" = "cas_number",
    "concentration_type_codes" = NULL,
    "concentration_unit_codes" = NULL,
    "control_type_codes" = NULL,
    "dose_stat_method_codes" = NULL,
    "duration_unit_codes" = NULL, 
    "effect_codes" = "code",
    "endpoint_assigned_codes" = NULL,
    "endpoint_codes" = NULL,
    "exposure_type_codes" = "code",
    "field_study_type_codes" = "code",
    "gender_codes" = "code",
    "geographic_codes" = NULL,
    "habitat_codes" = NULL,
    "ion_codes" = NULL,
    "lifestage_codes" = "code",
    "measurement_codes" = "code",
    "media_char_unit_codes" = NULL,
    "media_type_codes" = "code",
    "organic_matter_type_codes" = NULL,
    "organism_source_codes" = "code",
    "radio_label_codes" = NULL,
    "references" = "reference_number", # make sure tbl name isnt issue
    "response_site_codes" = "code",
    "sample_size_unit_codes" = NULL,
    "season_codes" = NULL,
    "species_synonyms" = NULL, 
    "species" = "species_number",
    "statistical_significance_codes" = NULL,
    "substrate_codes" = NULL,
    "test_location_codes" = "code",
    "test_method_codes" = "code",
    "test_type_codes" = "code",
    "trend_codes" = "code",
    "weight_unit_codes" = NULL
  )
}



