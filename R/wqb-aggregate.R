#' Aggregate Data for each Species
#'
#' Aggregate data to select the most sensitive value for each species. The 
#' priority of effect level depends on the benchmark method that will be used.
#'
#' @param data A data frame
#' @return Data frame
#' @export
#' @details To determine the most sensitive value:
#' 
#' - The species, life stage and effect are grouped together.
#' - Within each group prioritize the selection of the effect level using the following order which is specific to benchmark method:
#'   - SSD method use: 
#'      - EC/IC x<=10 > EC/IC11-EC/IC20 > MATC > NOEC/NOEL > LOEC/LOEL/MCIG> EC/ICx>20  > LC x<20 > LC x≥20
#'   - Deterministic method use: 
#'      - EC/IC10 > EC/IC11-EC/IC20 > MATC > LOEC/LOEL/MCIG > EC/ICx>20  > LC x<20 > LC x≥20 > NOEC/NOEL
#' - If multiple data points within each group exists then the geometric mean is calculated.
#' - The lowest concentration value is then selected.
#'  
#'  Only a subset of columns are returned since the data has been aggregated 
#'  down to the species level. 
#' 
#' @examples
#' \dontrun{
#' data <- wqb_aggregate(data)
#' }
wqb_aggregate <- function(data) {
  chk::check_data(
    data, 
    list(
      species_number = 1L,
      lifestage_description = "",
      effect_description = "",
      conc1_mean_std_effect = 1,
      method = ""
    )
  ) 
  
  method <- unique(data$method)
  
  if (length(method) != 1) {
    stop("Only 1 method allowed per data set")
  }
  
  if (method == "SSD") {
    grouped_data <- data |>
      dplyr::group_by(.data$species_number, .data$lifestage_description, .data$effect_description) |>
      dplyr::mutate(
        endpoint_alpha = stringr::str_extract(.data$endpoint, "[:alpha:]+"),
        endpoint_numeric = stringr::str_extract(.data$endpoint, "[:digit:]+"),
        effect_priority_order = dplyr::case_when(
          stringr::str_detect(.data$endpoint_alpha, "EC|IC") & .data$endpoint_numeric <= 10  ~ 8,
          stringr::str_detect(.data$endpoint_alpha, "EC|IC") & .data$endpoint_numeric >= 11 &  .data$endpoint_numeric <= 20  ~ 7,
          stringr::str_detect(.data$endpoint_alpha, "MATC") ~ 6,
          stringr::str_detect(.data$endpoint_alpha, "(NOEC)|(NOEL)") ~ 5,
          stringr::str_detect(.data$endpoint_alpha, "(LOEC)|(LOEL)|(MCIG)") ~ 4,
          stringr::str_detect(.data$endpoint_alpha, "EC|IC") & .data$endpoint_numeric > 20  ~ 3,
          stringr::str_detect(.data$endpoint_alpha, "LC") & .data$endpoint_numeric < 20  ~ 2,
          stringr::str_detect(.data$endpoint_alpha, "LC") & .data$endpoint_numeric >= 20  ~ 1
        ),
        group_id = dplyr::cur_group_id(),
        id = 1:dplyr::n()
      ) 
  } else {
    # Deterministic method
    grouped_data <- data |>
      dplyr::group_by(.data$species_number, .data$lifestage_description, .data$effect_description) |>
      dplyr::mutate(
        endpoint_alpha = stringr::str_extract(.data$endpoint, "[:alpha:]+"),
        endpoint_numeric = stringr::str_extract(.data$endpoint, "[:digit:]+"),
        effect_priority_order = dplyr::case_when(
          stringr::str_detect(.data$endpoint_alpha, "EC|IC") & .data$endpoint_numeric <= 10  ~ 8,
          stringr::str_detect(.data$endpoint_alpha, "EC|IC") & .data$endpoint_numeric >= 11 &  .data$endpoint_numeric <= 20  ~ 7,
          stringr::str_detect(.data$endpoint_alpha, "MATC") ~ 6,
          stringr::str_detect(.data$endpoint_alpha, "(LOEC)|(LOEL)|(MCIG)") ~ 5,
          stringr::str_detect(.data$endpoint_alpha, "EC|IC") & .data$endpoint_numeric > 20  ~ 4,
          stringr::str_detect(.data$endpoint_alpha, "LC") & .data$endpoint_numeric < 20  ~ 3,
          stringr::str_detect(.data$endpoint_alpha, "LC") & .data$endpoint_numeric >= 20  ~ 2,
          stringr::str_detect(.data$endpoint_alpha, "(NOEC)|(NOEL)") ~ 1,
        ),
        group_id = dplyr::cur_group_id(),
        id = 1:dplyr::n()
      ) 
  }
  
  aggregated_data <- 
    grouped_data |>
    dplyr::filter(.data$effect_priority_order == max(.data$effect_priority_order)) |>
    dplyr::mutate(
      conc1_mean_std_effect_aggr = geometric_mean(.data$conc1_mean_std_effect)
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
      "species_number", "latin_name", "common_name",  
      "ecological_group_class", "ecological_group", 
      "species_present_in_bc"
    )
  
  aggregated_data
}