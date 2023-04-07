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

#' Join Data 
#'
#' Join database tables together and start filtering and cleaning data.
#'
#' @param database A string to the location of the database.
#' @param quiet Turn off message when quiet set to TRUE.
#' @return Invisible data frame
#' @details Check the resource document more details on the data added, 
#' filter conditions and cleaning steps. This is part of Step 1. 
#'
#' @examples
#' \dontrun{
#' data_compiled <- wqb_join_data(
#'  database = "ecotox_ascii_09_15_2022.sqlite"
#' ) 
#' 
#' data_compiled <- wqb_join_data(
#'  database = "ecotox_db/ecotox_ascii_09_15_2022.sqlite"
#' ) 
#' }
wqb_join_data <- function(database, quiet = FALSE) {
  chk::chk_file(database)
  chk::chk_ext(database, "sqlite")
  if (!quiet) {
    message("Compile data")
  }
  
  on.exit(DBI::dbDisconnect(con))
  con  <- DBI::dbConnect(
    RSQLite::SQLite(),
    database
  )
  
  db_results <- DBI::dbReadTable(con, "results") |>
    dplyr::rename(additional_comments_results = "additional_comments")
  db_tests <- DBI::dbReadTable(con, "tests") |>
    dplyr::rename(additional_comments_tests = "additional_comments") |>
    dplyr::mutate(test_cas = as.character(test_cas))
  
  db_endpoint_codes <- DBI::dbReadTable(con, "endpoint_codes") |>
    dplyr::mutate(
      concentration_flag = as.logical(as.numeric(.data$concentration_flag))
    ) |>
    tibble::tibble()
  db_species <- DBI::dbReadTable(con, "species") |>
    dplyr::mutate(
      species_present_in_bc = as.logical(
        as.numeric(.data$species_present_in_bc)
      )
    ) |>
    tibble::tibble()
  db_lifestage_codes <- DBI::dbReadTable(con, "lifestage_codes") |>
    dplyr::rename("lifestage_description" = "description") |>
    tibble::tibble()
  db_chemicals <- DBI::dbReadTable(con, "chemicals") |>
    dplyr::mutate(
      present_in_bc_wqg = as.logical(as.numeric(.data$present_in_bc_wqg))
    ) |>
    tibble::tibble()
  db_duration_unit_codes <- DBI::dbReadTable(con, "duration_unit_codes") |>
    dplyr::mutate(
      duration_units_to_keep = as.logical(as.numeric(.data$duration_units_to_keep))
    ) |>
    dplyr::rename("duration_unit_description" = "description") |>
    tibble::tibble()
  db_concentration_unit_codes <- DBI::dbReadTable(con, "concentration_unit_codes") |>
    dplyr::mutate(
      conc_conversion_flag = as.logical(as.numeric(.data$conc_conversion_flag))
    ) |>
    dplyr::rename("concentration_unit_description" = "description") |>
    tibble::tibble()
  
  db_references <- DBI::dbReadTable(con, "references")
  db_effect_codes <- DBI::dbReadTable(con, "effect_codes") |>
    dplyr::rename("effect_description" = "description") |>
    tibble::tibble()
  db_media_type_codes <- DBI::dbReadTable(con, "media_type_codes") |>
    dplyr::rename("media_description" = "description") |>
    tibble::tibble()
  
  db_meta_data_download <- DBI::dbReadTable(con, "meta_data_dl")
  
  data <- join_data(db_results, db_tests, db_endpoint_codes, db_species,
                    db_lifestage_codes, db_chemicals, db_duration_unit_codes,
                    db_concentration_unit_codes, db_references,
                    db_effect_codes, db_media_type_codes,
                    db_meta_data_download)
  
  data
}

#' Join Data 
#'
#' Internal to wqb_join_data() to allow for testing. 
#'
#' @param db_results A data frame
#' @param db_tests A data frame
#' @param db_endpoint_codes A data frame
#' @param db_species A data frame
#' @param db_lifestage_codes A data frame
#' @param db_chemicals A data frame
#' @param db_duration_unit_codes A data frame
#' @param db_concentration_unit_codes A data frame
#' @param db_references A data frame
#' @param db_effect_codes A data frame
#' @param db_media_type_codes A data frame
#' @param db_meta_data_download A data frame
#' @return Invisible data frame
#'
#' @examples
#' \dontrun{
#' data <- wqb_join_data(
#'  db_results, db_tests, db_endpoint_codes, db_species, db_lifestage_codes, 
#'  db_chemicals, db_duration_unit_codes, db_concentration_unit_codes, 
#'  db_references, db_effect_codes, db_media_type_codes, db_meta_data_download
#' ) 
#' }
join_data <- function(db_results, db_tests, db_endpoint_codes, db_species,
                      db_lifestage_codes, db_chemicals, db_duration_unit_codes,
                      db_concentration_unit_codes, db_references,
                      db_effect_codes, db_media_type_codes,
                      db_meta_data_download) {
  
  joined_data <- db_results |>
    # filter to only water  (aquatic) tests
    dplyr::left_join(db_tests, by = "test_id") |>
    dplyr::filter(.data$organism_habitat == "Water") |>
    # filter to on conc endpoints
    dplyr::mutate(endpoint = stringr::str_replace_all(.data$endpoint, "(/)", "")) |>
    dplyr::left_join(db_endpoint_codes, by = c("endpoint" = "code")) |>
    dplyr::filter(.data$concentration_flag) |>
    # add species info
    dplyr::left_join(db_species, by = "species_number") |>
    # add life stage info
    dplyr::left_join(
      db_lifestage_codes, by = c("organism_lifestage" = "code")
    ) |>
    # add chemical info
    dplyr::left_join(db_chemicals, by = c("test_cas" = "cas_number")) |>
    # add duration unit info
    # use study duration but if study duration is missing use observed duration
    dplyr::mutate(
      duration_mean = dplyr::if_else(
        !is.na(.data$study_duration_mean),
        .data$study_duration_mean,
        .data$obs_duration_mean
      ),
      duration_unit = dplyr::if_else(
        !is.na(.data$study_duration_mean),
        .data$study_duration_unit,
        .data$obs_duration_unit
      ),
      duration_unit = dplyr::if_else(
        !stringr::str_detect(.data$duration_mean, "NC"),
        .data$duration_unit,
        .data$obs_duration_unit
      ),
      duration_mean = dplyr::if_else(
        !stringr::str_detect(.data$duration_mean, "NC"),
        .data$duration_mean,
        .data$obs_duration_mean
      )
    ) |>
    dplyr::left_join(
      db_duration_unit_codes, by = c("duration_unit" = "code")
    ) |>
    # add concentration unit info
    dplyr::left_join(
      db_concentration_unit_codes, by = c("conc1_unit" = "code")
    ) |>
    # add reference info
    dplyr::left_join(db_references, by = c("reference_number")) |>
    # add effect info
    # clean codes so they join
    dplyr::mutate(effect = stringr::str_replace_all(.data$effect, "(/)|(~)", "")) |>
    dplyr::left_join(db_effect_codes, by = c("effect" = "code")) |>
    # add media groups
    # clean codes so they join
    dplyr::mutate(media_type = stringr::str_replace_all(.data$media_type, "(/)", "")) |>
    dplyr::left_join(db_media_type_codes, by = c("media_type" = "code")) |>
    # add meta info
    dplyr::mutate(
      download_date = db_meta_data_download$download_date,
      version = stringr::str_squish(db_meta_data_download$version)
    ) |>
    tibble::tibble() |>
    dplyr::select(
      "chemical_name", 
      "test_cas",
      "test_id", 
      "result_id", 
      "endpoint", 
      "effect", 
      "effect_description",
      "conc1_mean", 
      "conc1_unit", 
      "conc_conversion_flag", 
      "conc_conversion_value_multiplier", 
      "conc_conversion_unit",
      "conc2_mean", 
      "conc2_unit",
      "conc3_mean", 
      "conc3_unit",
      "duration_mean", 
      "duration_unit",
      "duration_units_to_keep", 
      "duration_value_multiplier_to_hours", 
      "study_duration_mean", 
      "study_duration_unit",
      "obs_duration_mean",
      "obs_duration_unit", 
      "organism_habitat",
      "species_number", 
      "latin_name", 
      "common_name", 
      "kingdom", 
      "phylum_division", 
      "subphylum_div", 
      "superclass",
      "class", 
      "tax_order", 
      "family", 
      "genus", 
      "species", 
      "subspecies", 
      "variety",
      "species_present_in_bc", 
      "ecological_group", 
      "trophic_group",
      "lifestage_description", 
      "simple_lifestage", 
      "media_type", 
      "media_description", 
      "media_type_group",
      "present_in_bc_wqg", 
      "reference_number", 
      "author", 
      "title", 
      "source", 
      "publication_year",
      "additional_comments_tests", 
      "additional_comments_results",
      "download_date", 
      "version"
    )
  
  joined_data
  
}

