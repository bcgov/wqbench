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
      trophic_group = factor(""),
      ecological_group = factor(""),
      duration_hrs = 1,
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
        stringr::str_detect(trophic_group, "(?i)^amphibian$|^fish$")  & duration_hrs <= 96 ~ "acute",
        stringr::str_detect(trophic_group, "(?i)^amphibian$|^fish$") & stringr::str_detect(simple_lifestage, "(?i)^juvenile$|(?i)^adult$")  & duration_hrs >= 504 ~ "chronic",
        stringr::str_detect(trophic_group, "(?i)^amphibian$|^fish$") & stringr::str_detect(simple_lifestage, "(?i)^els$")  & duration_hrs >= 168 ~ "chronic",
        # Invertebrates
        stringr::str_detect(trophic_group, "(?i)^invertebrate$") & duration_hrs <= 96 ~ "acute",
        stringr::str_detect(trophic_group, "(?i)^invertebrate$") & stringr::str_detect(ecological_group, "(?i)Planktonic Invertebrate") & duration_hrs > 96 ~ "chronic",
        stringr::str_detect(trophic_group, "(?i)^invertebrate$") & stringr::str_detect(ecological_group, "(?i)Other") & duration_hrs >= 168 ~ "chronic",
        # Algae
        stringr::str_detect(trophic_group, "(?i)^algae$") & duration_hrs <= 24 ~ "acute",
        stringr::str_detect(trophic_group, "(?i)^algae$") & duration_hrs > 24 ~ "chronic",
        # Plants
        stringr::str_detect(trophic_group, "(?i)^plant$") & duration_hrs <= 48 ~ "acute",
        stringr::str_detect(trophic_group, "(?i)^plant$") & duration_hrs > 168 ~ "chronic",
        # anything outside the specified category is set as acute
        TRUE ~ "acute"
      ) 
    ) |>
    dplyr::select(
      "chemical_name", 
      "cas",
      "endpoint", 
      "effect",
      "conc1_mean_std_mg.L",
      "duration_hrs", 
      "duration_class",
      "organism_habitat",
      "species_number", 
      "latin_name", 
      "common_name", 
      "species_present_in_bc", 
      "ecological_group", 
      "trophic_group",
      "lifestage", 
      "media_type", 
      "present_in_bc_wqg", 
      "author", 
      "title", 
      "source", 
      "publication_year",
      "download_date",
      "version"
    )
  
  data_classified
}