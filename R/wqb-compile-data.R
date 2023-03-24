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

#' Compile Data 
#'
#' Join database tables together and start filtering and cleaning data.
#'
#' @param database A string to the location of the database.
#' @return Invisible data frame
#' @export
#' @details Check the resource document more details on the data added, 
#' filter conditions and cleaning steps. This is Step 1. 
#'
#' @examples
#' \dontrun{
#' data_compiled <- wqb_compile_dataset(
#'  database = "ecotox_ascii_09_15_2022.sqlite"
#' ) 
#' 
#' data_compiled <- wqb_compile_dataset(
#'  database = "ecotox_db/ecotox_ascii_09_15_2022.sqlite"
#' ) 
#' }
wqb_compile_dataset <- function(database) {
  chk::chk_file(database)
  chk::chk_ext(database, "sqlite")
  
  on.exit(DBI::dbDisconnect(con))
  con  <- DBI::dbConnect(
    RSQLite::SQLite(),
    database
  )
  
  db_results <- DBI::dbReadTable(con, "results") |>
    dplyr::rename(additional_comments_results = "additional_comments")
  db_tests <- DBI::dbReadTable(con, "tests") |>
    dplyr::rename(additional_comments_tests = "additional_comments")
  
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
  
  combined_data <- db_results |>
    # filter to only water  (aquatic) tests
    dplyr::left_join(db_tests, by = "test_id") |>
    dplyr::filter(.data$organism_habitat == "Water") |>
    # filter to on conc endpoints
    dplyr::left_join(db_endpoint_codes, by = c("endpoint" = "code")) |>
    dplyr::filter(.data$concentration_flag) |>
    # clean up asterick endpoints
    dplyr::mutate(endpoint = stringr::str_replace(.data$endpoint, "\\*", "")) |>
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
    tibble::tibble()

  chk::chk_not_missing(combined_data$organism_lifestage)
  
  # select columns
  selected_columns_data <- combined_data |>
    dplyr::select(
      "chemical_name", "test_cas",
      "test_id", "result_id", "endpoint", "effect", "effect_description",
      "conc1_mean", "conc1_unit", 
      "conc_conversion_flag", "conc_conversion_value_multiplier", 
      "conc_conversion_unit",
      "conc2_mean", "conc2_unit",
      "conc3_mean", "conc3_unit",
      "duration_mean", "duration_unit",
      "duration_units_to_keep", "duration_value_multiplier_to_hours", 
      "study_duration_mean", "study_duration_unit",
      "obs_duration_mean", "obs_duration_unit", 
      "organism_habitat",
      "species_number", "latin_name", "common_name", "kingdom", 
      "phylum_division", "subphylum_div", "superclass", "class", "tax_order", 
      "family", "genus", "species", "subspecies", "variety",
      "species_present_in_bc", 
      "ecological_group_class", "ecological_group",
      "lifestage_description", "simple_lifestage", 
      "media_type", "media_description", "media_type_group",
      "present_in_bc_wqg", 
      "reference_number", "reference_type", "author", "title", "source", 
      "publication_year",
      "additional_comments_tests", "additional_comments_results"
    )
  
  compiled_data <- selected_columns_data |>
    # remove missing concentrations
    dplyr::filter(!(.data$conc1_mean == "NR")) |>
    # remove concentration with < or > in them
    dplyr::filter(!(stringr::str_detect(.data$conc1_mean, "\\<|\\>"))) |>
    # remove rows with no species genus
    dplyr::filter(!(.data$genus == "")) |>
    # remove rows with no ecological group 
    dplyr::filter(!(is.na(.data$ecological_group))) |>
    # remove rows with no duration value
    dplyr::filter(!(.data$duration_mean == "")) |> 
    dplyr::filter(!(.data$duration_mean == "NR")) |>
    dplyr::filter(!(is.na(.data$duration_mean))) |>
    # remove rows where duration can not be standardized 
    dplyr::filter(.data$duration_units_to_keep) |>
    # remove rows where concentration cannot be standardized 
    dplyr::filter(.data$conc_conversion_flag) |>
    dplyr::mutate(
      # remove asterisk from end point
      endpoint = stringr::str_replace(.data$endpoint, "\\*", ""),
      # remove asterisk from conc1_mean values and convert to numeric
      conc1_mean = stringr::str_replace(.data$conc1_mean, "\\*", ""),
      conc1_mean = as.numeric(.data$conc1_mean),
      # convert duration units to hours
      duration_mean = as.numeric(.data$duration_mean),
      duration_mean_std = .data$duration_mean * .data$duration_value_multiplier_to_hours,
      duration_unit_std = "hours",
      # convert concentration units to mg/L or ppm
      conc1_mean_std = .data$conc1_mean * .data$conc_conversion_value_multiplier,
      # missing (NA) lifestages should be coded as adult
      # doesn't appear to be any missing but adding in just in case
      simple_lifestage = dplyr::if_else(
        is.na(.data$lifestage_description), 
        "adult", 
        .data$simple_lifestage
      ),
      # simple life stage should only match for amphibians and fish
      simple_lifestage = dplyr::case_when(
        .data$ecological_group == "Invertebrate" ~ NA_character_,
        .data$ecological_group == "Algae" ~ NA_character_,
        .data$ecological_group == "Plant" ~ NA_character_,
        TRUE ~ .data$simple_lifestage
      ),
      # set cas nums to be char as values too large to be ints
      test_cas = as.character(.data$test_cas),
      # set factor levels
      ecological_group = factor(
        .data$ecological_group, 
        levels = sort(unique(.data$ecological_group))
      ),
      ecological_group_class = factor(
        .data$ecological_group_class, 
        levels = sort(unique(.data$ecological_group_class))
      )
    ) |>
    dplyr::select(
      "chemical_name", "test_cas",
      "test_id", "result_id", "endpoint", "effect", "effect_description",
      "conc1_mean", "conc1_unit", 
      "conc_conversion_flag", "conc_conversion_value_multiplier", 
      "conc1_mean_std",
      "conc_conversion_unit",
      "conc2_mean", "conc2_unit",
      "conc3_mean", "conc3_unit",
      "duration_mean_std", "duration_unit_std",
      "duration_mean", "duration_unit",
      "duration_units_to_keep", "duration_value_multiplier_to_hours", 
      "study_duration_mean", "study_duration_unit",
      "obs_duration_mean", "obs_duration_unit", 
      "organism_habitat",
      "species_number", "latin_name", "common_name", "kingdom", 
      "phylum_division", "subphylum_div", "superclass", "class", "tax_order", 
      "family", "genus", "species", "subspecies", "variety",
      "species_present_in_bc", 
      "ecological_group_class", "ecological_group",
      "lifestage_description", "simple_lifestage", 
      "media_type", "media_description", "media_type_group",
      "present_in_bc_wqg", 
      "reference_number", "reference_type", "author", "title", "source", 
      "publication_year",
      "additional_comments_tests", "additional_comments_results"
    )
  
  compiled_data
}
