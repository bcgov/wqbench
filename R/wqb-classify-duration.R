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
#' Each test will be classified as an acute or chronic and a new column called 
#' `duration_class` will be added to the data set. 
#'
#' @param data A data frame of the compiled data set.
#' @return Data frame
#' @export
#' @details The data set passed should be the output of the 
#'   ` wqb_compile_dataset()` function and must at least contain the columns:
#'    `ecological_group`, `duration_mean_std` and `simple_lifestage`.
#' 
#'   Classifications for acute and chronic are separated by trophic group
#'   (`ecological_group`). The values are classified as acute, chronic in a new 
#'   column called `duration_class`.
#'
#'   Values not meeting this criteria are classified as acute.
#'  
#'  Fish and amphibians
#'    Acute: 
#'      - duration ≤ 96 hours
#'    Chronic:
#'      - life stage (`simple_lifestage`) is juvenile or adult and 
#'      duration ≥ 504 hours
#'      - life stage (`simple_lifestage`) is early life stages (els) and 
#'      duration ≥ 168 hours
#'  
#'  Aquatic invertebrates
#'    Acute: 
#'      - duration ≤ 96 hours
#'    Chronic:
#'      - Planktonic invertebrates (`ecological_group_class`) and duration > 96 hours
#'      - Other invertebrates (`ecological_group_class`) and duration ≥ 168 hours
#'  
#'  Algae
#'    Acute: 
#'     - duration ≤ 24 hours
#'    Chronic: 
#'     - duration > 24 hours
#'    
#'  Aquatic Plants
#'    Acute: 
#'     - duration ≤ 48 hours 
#'    Chronic: 
#'    - duration > 7 days (used Lemna protocol as a guide, EC 2007)
#' 
#' Reference
#'  Environment Canada 2007. Test for measuring the inhibition of growth using 
#'  the freshwater macrophyte, Lemna minor.  Available online at: 
#'  https://publications.gc.ca/collections/collection_2013/ec/En49-7-1-37-eng.pdf
#'
#' @examples
#' \dontrun{
#' classified_data <- wqb_classify_duration(compiled_data)
#' }
wqb_classify_duration <- function(data) {
  chk::check_data(
    data, 
    list(
      ecological_group = factor(""),
      duration_mean_std = 1,
      simple_lifestage = c("", NA)
    )
  ) 
  
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
      "duration_class",
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
  
  data_classified
}