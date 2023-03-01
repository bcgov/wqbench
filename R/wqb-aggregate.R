#' Aggregate Data for each Species
#' 
#' Aggregate data so the most sensitive value is selected for each species.
#'
#' @param data A data frame that is the output of the `wqb_standardize_effect()` function.
#' @param cas_num A string of the cas number. 
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
#' wqb_aggregate(data, "7553562")
#' wqb_aggregate(data, "67663")
#' 
#' aggregated_data <- wqb_aggregate(standardized_effect_data, "71432")
#' }
wqb_aggregate <- function(data, cas_num) {
  chk::check_data(
    data, 
    list(
    )
  ) 
  chk::chk_character_or_factor(cas_num)
  
  aggregated_data <- data |>
    dplyr::filter(.data$test_cas == cas_num) |>
    dplyr::group_by(.data$species_number, .data$lifestage_description, .data$effect_description) |>
    dplyr::mutate(
      endpoint_alpha = stringr::str_extract(.data$endpoint, "[:alpha:]+"),
      endpoint_numeric = stringr::str_extract(.data$endpoint, "[:digit:]+"),
      effect_priority_order = dplyr::case_when(
        stringr::str_detect(.data$endpoint_alpha, "EC|IC") & .data$endpoint_numeric <= 10  ~ 8,
        stringr::str_detect(.data$endpoint_alpha, "EC|IC") & .data$endpoint_numeric > 10 &  .data$endpoint_numeric <= 20  ~ 7,
        stringr::str_detect(.data$endpoint_alpha, "MATC") ~ 6,
        stringr::str_detect(.data$endpoint_alpha, "(NOEC)|(NOEL)") ~ 5,
        stringr::str_detect(.data$endpoint_alpha, "(LOEC)|(LOEL)|(MCIG)") ~ 4,
        stringr::str_detect(.data$endpoint_alpha, "EC|IC") & .data$endpoint_numeric > 20  ~ 3,
        stringr::str_detect(.data$endpoint_alpha, "LC") & .data$endpoint_numeric < 20  ~ 2,
        stringr::str_detect(.data$endpoint_alpha, "LC") & .data$endpoint_numeric >= 20  ~ 1
      ),
      group_id = dplyr::cur_group_id(),
      id = 1:dplyr::n()
    ) |>
    dplyr::filter(.data$effect_priority_order == min(.data$effect_priority_order)) |>
    dplyr::mutate(
      conc1_mean_std_effect_aggr = mean(.data$conc1_mean_std_effect)
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$species_number) |>
    dplyr::arrange(.data$conc1_mean_std_effect_aggr) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$species_number) |>
    dplyr::select(
      "chemical_name", "test_cas",
      "effect", "effect_description",
      "conc1_mean_std_effect_aggr", "conc_conversion_unit",
      "species_number", "latin_name", "common_name"
    )
  
  aggregated_data
}