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
#' Determine and apply factor needed to standardize the endpoints. 
#'
#' @param data A data frame
#' @param quiet Turn off message when quiet set to TRUE.
#' @return A data frame
#' @export
#' @details Check the resource document for details the rules used to determine 
#'  standardized the endpoints. This is Step 3. 
#' 
#' @examples
#' \dontrun{
#' standardized_effect_data <- wqb_standardize_effect(data)
#' standardized_effect_data <- wqb_standardize_effect(classified_data)
#' }
wqb_standardize_effect <- function(data, quiet = FALSE) {
  chk::check_data(
    data, 
    list(
      endpoint = "",
      duration_class = "",
      ecological_group = factor("")
    )
  ) 
  if (!quiet) {
    message("Standardize type of effect")
  }
  
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
      "cas",
      "latin_name", 
      "common_name",
      "endpoint", 
      "effect",
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
      "species_number",
      "download_date",
      "version"
    )
  
  effect_std 
}