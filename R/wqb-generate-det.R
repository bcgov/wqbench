#' Generate the Variation Factor Benchmark Guideline Value
#'
#' Finds the lowest concentration and then divides by the assessment
#' factor to output the benchmark value for the chemical.
#'
#' @param data A data frame 
#'
#' @return A data frame which contains the lowest concentration, the total 
#'  assessment factor and the benchmark value.
#' @export
#'
#' @examples
#' \dontrun{
#' bench_iodine <- wqb_generate_det(data)
#' }
wqb_generate_det <- function(data) {
  chk::check_data(
    data,
    list(
      method = c("Deterministic", "Deterministic", "Deterministic"),
      sp_aggre_conc_mg.L = 1
    )
  )
  af <- gen_af_value(data)
  value <- data |>
    dplyr::arrange(.data$sp_aggre_conc_mg.L) |>
    dplyr::slice(1) |>
    dplyr::select("sp_aggre_conc_mg.L") |>
    dplyr::mutate(
      ctv_est_mg.L = .data$sp_aggre_conc_mg.L,
      ctv_lcl_mg.L = NA_real_,
      ctv_ucl_mg.L = NA_real_
    ) |>
    dplyr::select(
      "ctv_est_mg.L", "ctv_lcl_mg.L", "ctv_ucl_mg.L"
    )
  value 
}
