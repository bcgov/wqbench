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
      ecological_group = ""
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
      "chemical_name", "test_cas",
      "test_id", "result_id", "endpoint", "effect", "effect_description",
      "conc1_mean", "conc1_unit", 
      "conc_conversion_flag", "conc_conversion_value_multiplier", 
      "conc1_mean_std",
      "conc1_mean_std_effect",
      "conc_conversion_unit",
      "conc1_mean_std_effect_divide_factor", 
      "conc2_mean", "conc2_unit",
      "conc3_mean", "conc3_unit",
      "obs_duration_mean", "obs_duration_unit",
      "obs_duration_mean_std", "obs_duration_unit_std",
      "study_duration_mean", "study_duration_unit",
      "exposure_duration_mean", "exposure_duration_unit",
      "duration_units_to_keep", "duration_value_multiplier_to_hours", 
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
  
  effect_std 
}