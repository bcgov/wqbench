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
#' bench_iodine <- wqb_generate_vf(data)
#' }
wqb_generate_vf <- function(data) {
  chk::check_data(
    data,
    list(
      method = c("VF", "VF", "VF"),
      conc1_mean_std_effect_aggr = 1
    )
  )
  af <- gen_af_value(data)
  value <- data |>
    dplyr::arrange(.data$conc1_mean_std_effect_aggr) |>
    dplyr::slice(1) |>
    dplyr::select("conc1_mean_std_effect_aggr") |>
    dplyr::mutate(
      af = af,
      benchmark_est = .data$conc1_mean_std_effect_aggr / .data$af,
      benchmark_lcl = NA_real_,
      benchmark_ucl = NA_real_
    ) |>
    dplyr::select("benchmark_est", "benchmark_lcl", "benchmark_ucl", "af")
  value 
}
