#' Aggregate Data for each Species
#' 
#' Aggregate data so the most sensitive value is selected for each species.
#'
#' @param data A data frame that is the output of the `wqb_standardize_effect()` function.
#' @param chemical A string of the chemical name or cas number. 
#' @return Data frame
#' @export
#' @details 
#'  Each species, life stage and effect are group for each chemical and the 
#'  most sensitive effect concentration is selected for each speices. 
#'  
#'  If multiple data points within each group exists then the geometric mean is 
#'  calculated. 
#'
#' @examples
#' \dontrun{
#' 
#' 
#' }
wqb_aggregate <- function(data, chemical) {
  chk::check_data(
    data, 
    list(
    )
  ) 
  chk::chk_character_or_factor(chemical)
  
  aggregated_data <- data |>
    dplyr::filter(test_cas == chemical) |>
    dplyr::group_by(species_number, lifestage_description, effect_description) |>
    dplyr::mutate(
      endpoint_alpha = stringr::str_extract(.data$endpoint, "[:alpha:]+"),
      endpoint_numeric = stringr::str_extract(.data$endpoint, "[:digit:]+"),
      effect_priority_order = dplyr::case_when(
        stringr::str_detect(endpoint_alpha, "EC|IC") & endpoint_numeric <= 10  ~ 8,
        stringr::str_detect(endpoint_alpha, "EC|IC") & endpoint_numeric > 10 &  endpoint_numeric <= 20  ~ 7,
        stringr::str_detect(endpoint_alpha, "MATC") ~ 6,
        stringr::str_detect(endpoint_alpha, "(NOEC)|(NOEL)") ~ 5,
        stringr::str_detect(endpoint_alpha, "(LOEC)|(LOEL)|(MCIG)") ~ 4,
        stringr::str_detect(endpoint_alpha, "EC|IC") & endpoint_numeric > 20  ~ 3,
        stringr::str_detect(endpoint_alpha, "LC") & endpoint_numeric < 20  ~ 2,
        stringr::str_detect(endpoint_alpha, "LC") & endpoint_numeric >= 20  ~ 1
      ),
      group_id = dplyr::cur_group_id(),
      id = 1:dplyr::n()
    ) |>
    dplyr::filter(effect_priority_order == min(effect_priority_order)) |>
    dplyr::mutate(
      conc1_mean_std_effect_aggr = mean(conc1_mean_std_effect)
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(species_number) |>
    dplyr::arrange(conc1_mean_std_effect_aggr) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::arrange(species_number) |>
    dplyr::relocate(
      species_number, lifestage_description, effect_description, group_id, id, 
      conc1_mean_std_effect, conc1_mean_std_effect_aggr, 
      endpoint, effect_priority_order, 
      common_name,
      .after = chemical_name
    ) 
  
  aggregated_data
}