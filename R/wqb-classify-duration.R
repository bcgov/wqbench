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

#' Classify Duration
#'
#' Classify each test as either acute or chronic in the `duration_class`
#' column.
#'
#' @param data A data frame
#' @param quiet Turn off message when quiet set to TRUE.
#' @return A data frame
#' @export
#' @details Check the resource document for the rules used to determine the 
#' classification for each trophic group. This is Step 2. 
#'
#' @examples
#' \dontrun{
#' classified_data <- wqb_classify_duration(compiled_data)
#' }
wqb_classify_duration <- function(data, quiet = FALSE) {
  chk::check_data(
    data, 
    list(
      ecological_group = factor(""),
      ecological_group_class = "",
      duration_mean_std = 1,
      simple_lifestage = c("", NA)
    )
  ) 
  if (!quiet) {
    message("Classify duration")
  }
  
  data_classified <- data |>
    dplyr::mutate(
      duration_class = dplyr::case_when(
        # Fish and Amphibians
        stringr::str_detect(ecological_group, "(?i)^amphibian$|^fish$")  & duration_mean_std <= 96 ~ "acute",
        stringr::str_detect(ecological_group, "(?i)^amphibian$|^fish$") & stringr::str_detect(simple_lifestage, "(?i)^juvenile$|(?i)^adult$")  & duration_mean_std >= 504 ~ "chronic",
        stringr::str_detect(ecological_group, "(?i)^amphibian$|^fish$") & stringr::str_detect(simple_lifestage, "(?i)^els$")  & duration_mean_std >= 168 ~ "chronic",
        # Invertebrates
        stringr::str_detect(ecological_group, "(?i)^invertebrate$") & duration_mean_std <= 96 ~ "acute",
        stringr::str_detect(ecological_group, "(?i)^invertebrate$") & stringr::str_detect(ecological_group_class, "(?i)Planktonic Invertebrate") & duration_mean_std > 96 ~ "chronic",
        stringr::str_detect(ecological_group, "(?i)^invertebrate$") & stringr::str_detect(ecological_group_class, "(?i)Other") & duration_mean_std >= 168 ~ "chronic",
        # Algae
        stringr::str_detect(ecological_group, "(?i)^algae$") & duration_mean_std <= 24 ~ "acute",
        stringr::str_detect(ecological_group, "(?i)^algae$") & duration_mean_std > 24 ~ "chronic",
        # Plants
        stringr::str_detect(ecological_group, "(?i)^plant$") & duration_mean_std <= 48 ~ "acute",
        stringr::str_detect(ecological_group, "(?i)^plant$") & duration_mean_std > 168 ~ "chronic",
        # anything outside the specified category is set as acute
        TRUE ~ "acute"
      ) 
    ) |>
    dplyr::select(
      "chemical_name", 
      "cas",
      "endpoint", 
      "effect",
      "conc1_mean", 
      "conc1_unit", 
      "conc_conversion_flag", 
      "conc_conversion_value_multiplier", 
      "conc1_mean_std",
      "conc_conversion_unit",
      "duration_mean_std", 
      "duration_unit_std",
      "duration_mean", 
      "duration_unit",
      "duration_units_to_keep", 
      "duration_value_multiplier_to_hours", 
      "study_duration_mean", 
      "study_duration_unit",
      "obs_duration_mean", 
      "obs_duration_unit", 
      "duration_class",
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
      "ecological_group_class", 
      "ecological_group",
      "lifestage_description", 
      "simple_lifestage", 
      "media_type", 
      "media_description", 
      "media_type_group",
      "present_in_bc_wqg", 
      "reference_number", 
      "reference_type", 
      "author", 
      "title", 
      "source", 
      "publication_year",
      "additional_comments_tests", 
      "additional_comments_results",
      "download_date",
      "version"
    )
  
  data_classified
}