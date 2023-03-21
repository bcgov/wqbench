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

#' Standardize type of Effect
#' 
#' Each concentration is divided by a factor to standardize the effect. The 
#' factor is determined from the set of rules provided in the details. 
#'
#' @param data A data frame that is the output of the `wqb_classify_duration()` 
#'  function.
#' @return Data frame
#' @export
#' @details The data argument passed to this function should be the output of
#'  `wqb_classify_duration()` and must at least contain the columns: 
#'  `endpoint`, `duration_class`, and `ecological_group`.
#'  
#'  The `conc1_mean_std` value is divided by the factor determined in the rules
#'  below and adds two columns to the data set 
#'  `conc1_mean_std_effect_divide_factor` and `conc1_mean_std_effect`.
#' 
#' If duration is acute AND effect type has (LOEC, LOEL, MCIG or EC/IC/LC x≥20) AND group has (fish, amphibians, inverts or plants) then divide by 10
#' If duration is acute AND effect type has (LOEC, LOEL, MCIG or EC/IC/LC x≥20) AND group has (algae) then divide by 5
#' If duration is acute AND effect type has (NOEC, NOEL, MATC or EC/IC/LC x<20) then divide by 5
#' If duration is chronic AND effect type has (LOEC, LOEL, MCIG or EC/IC/LC x≥20) then divide by 5
#' If duration is chronic AND effect type has (NOEC, NOEL, MATC or EC/IC/LC x<20) then divide by 1
#' 
#' @examples
#' \dontrun{
#' standardized_effect_data <- wqb_standardize_effect(data)
#' 
#' standardized_effect_data <- wqb_standardize_effect(classified_data)
#' }
wqb_standardize_effect <- function(data) {
  chk::check_data(
    data, 
    list(
      endpoint = "",
      duration_class = "",
      ecological_group = factor("")
    )
  ) 
  
  effect_std <- data |>
    dplyr::mutate(
      endpoint_alpha = stringr::str_extract(.data$endpoint, "[:alpha:]+"),
      endpoint_numeric = stringr::str_extract(.data$endpoint, "[:digit:]+"),
      conc1_mean_std_effect_divide_factor = dplyr::case_when(
        stringr::str_detect(.data$duration_class, "(?i)^acute$") & 
          stringr::str_detect(.data$ecological_group, "(?i)^amphibian$|^fish$|^invertebrate$|^plant$") & 
          (stringr::str_detect(.data$endpoint, "(LOEC)|(LOEL)|(MCIG)") | (stringr::str_detect(.data$endpoint, "(EC)|(IC)|(LC)") & .data$endpoint_numeric >= 20))  ~ 10L,
        
        stringr::str_detect(.data$duration_class, "(?i)^acute$") & 
          stringr::str_detect(.data$ecological_group, "(?i)^algae$") & 
          (stringr::str_detect(.data$endpoint, "(LOEC)|(LOEL)|(MCIG)") | (stringr::str_detect(.data$endpoint, "(EC)|(IC)|(LC)") & .data$endpoint_numeric >= 20))  ~ 5L,
        
        stringr::str_detect(.data$duration_class, "(?i)^acute$") & 
          (stringr::str_detect(.data$endpoint, "(NOEC)|(NOEL)|(MATC)") | (stringr::str_detect(.data$endpoint, "(EC)|(IC)|(LC)") & .data$endpoint_numeric < 20))  ~ 5L,
        
        stringr::str_detect(.data$duration_class, "(?i)^chronic$") & 
          (stringr::str_detect(.data$endpoint, "(LOEC)|(LOEL)|(MCIG)") | (stringr::str_detect(.data$endpoint, "(EC)|(IC)|(LC)") & .data$endpoint_numeric >= 20))  ~ 5L,
        
        stringr::str_detect(.data$duration_class, "(?i)^chronic$") & 
          (stringr::str_detect(.data$endpoint, "(NOEC)|(NOEL)|(MATC)") | (stringr::str_detect(.data$endpoint, "(EC)|(IC)|(LC)") & .data$endpoint_numeric < 20))  ~ 1L,
        
        TRUE ~ NA_integer_
      ),
      conc1_mean_std_effect = .data$conc1_mean_std / .data$conc1_mean_std_effect_divide_factor
    ) |>
    dplyr::select(
      "chemical_name", 
      "cas" = "test_cas",
      "latin_name", 
      "common_name",
      "endpoint", 
      "effect" = "effect_description",
      "effect_conc_mg.L" = "conc1_mean_std",
      "lifestage" = "lifestage_description", 
      "duration_hrs" = "duration_mean_std", 
      "duration_class",
      "effect_conc_std_mg.L" = "conc1_mean_std_effect",
      "ACR" = "conc1_mean_std_effect_divide_factor", 
      "media_type", 
      "trophic_group" = "ecological_group",
      "ecological_group" = "ecological_group_class", 
      "species_present_in_bc", 
      "author", 
      "title", 
      "source", 
      "publication_year",
      "present_in_bc_wqg",
      "species_number"
    )
  
  effect_std 
}