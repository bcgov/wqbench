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
#' @param data A data frame.
#' @param quiet Turn off message when quiet set to TRUE.
#' @return Invisible data frame
#' @details Check the resource document more details on the data added, 
#' filter conditions and cleaning steps. This is part of Step 1. 
#'
#' @examples
#' \dontrun{
#' data <- wqb_clean_data(data) 
#' }
wqb_clean_data <- function(data, quiet = FALSE) {
  if (!quiet) {
    message("Clean data")
  }
 
  compiled_data <- data |>
    # remove missing concentrations
    dplyr::filter(!(.data$conc1_mean == "NR")) |>
    # remove concentration with < or > in them
    dplyr::filter(!(stringr::str_detect(.data$conc1_mean, "\\<|\\>"))) |>
    # remove rows with no species genus
    dplyr::filter(!(.data$genus == "")) |>
    # remove rows with no ecological group 
    dplyr::filter(!(is.na(.data$trophic_group))) |>
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
      duration_hrs = .data$duration_mean * .data$duration_value_multiplier_to_hours,
      # convert concentration units to mg/L or ppm
      effect_conc_mg.L = .data$conc1_mean * .data$conc_conversion_value_multiplier,
      # missing (NA) lifestages should be coded as adult
      # doesn't appear to be any missing but adding in just in case
      simple_lifestage = dplyr::if_else(
        is.na(.data$lifestage_description), 
        "adult", 
        .data$simple_lifestage
      ),
      # simple life stage should only match for amphibians and fish
      simple_lifestage = dplyr::case_when(
        .data$trophic_group == "Invertebrate" ~ NA_character_,
        .data$trophic_group == "Algae" ~ NA_character_,
        .data$trophic_group == "Plant" ~ NA_character_,
        TRUE ~ .data$simple_lifestage
      ),
      # set cas nums to be char as values too large to be ints
      test_cas = as.character(.data$test_cas),
      # set factor levels
      trophic_group = factor(
        .data$trophic_group, 
        levels = sort(unique(.data$trophic_group))
      ),
      ecological_group = factor(
        .data$ecological_group, 
        levels = sort(unique(.data$ecological_group))
      )
    ) |>
    dplyr::select(
      "chemical_name", 
      "cas" = "test_cas",
      # "test_id", 
      #"result_id", 
      "endpoint", 
      #"effect", 
      "effect" = "effect_description",
      "conc1_mean", 
      "conc1_unit", 
      "conc_conversion_flag", 
      "conc_conversion_value_multiplier", 
      "effect_conc_mg.L",
      "conc_conversion_unit",
      #"conc2_mean", 
      #"conc2_unit",
      #"conc3_mean", 
      #"conc3_unit",
      "duration_hrs", 
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
      # "kingdom", 
      # "phylum_division", 
      # "subphylum_div", 
      # "superclass", 
      #"class", 
      #"tax_order", 
      #"family", 
      #"genus", 
      #"species", 
      #"subspecies", 
      #"variety",
      "species_present_in_bc", 
      "trophic_group", 
      "ecological_group", 
      "lifestage" = "lifestage_description", 
      "simple_lifestage", 
      "media_type", 
      # "media_description", 
      # "media_type_group", # I think this should remain not media_type
      "present_in_bc_wqg", 
      # "reference_number", 
      # "reference_type", 
      "author", 
      "title", 
      "source", 
      "publication_year",
      # "additional_comments_tests", 
      # "additional_comments_results",
      "download_date",
      "version"
    )
  
  compiled_data
}
