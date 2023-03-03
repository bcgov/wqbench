#' Generate the Benchmark Guideline Value using a SSD
#'
#' @param data A data frame of clean and processed data filtered to only a 
#'  single chemical.
#' @param fit The fit from ssd
#'
#' @return A data frame which contains the HC5 value, the total 
#'  assessment factor and the benchmark value.
#' @export
#'
#' @examples
#' \dontrun{
#'  bench_iodine <- wqb_generate_ssd(data, fit)
#' }
wqb_generate_ssd <- function(data, fit) {
  chk::check_data(
    data,
    list(
      method = c("SSD", "SSD", "SSD"),
      conc1_mean_std_effect_aggr = 1
    )
  )
  af <- gen_af_value(data)
  hc5 <- wqb_generate_ssd_hc5(fit)
  
  value <- hc5 |>
    dplyr::select("est", "lcl", "ucl") |>
    dplyr::mutate(
      af = af,
      benchmark_est = .data$est / .data$af,
      benchmark_lcl = .data$lcl / .data$af,
      benchmark_ucl = .data$ucl / .data$af,
    )
  
  value
}



#' Fit a SSD
#'
#' @param data A data frame of clean and processed data filtered to only a 
#'  single chemical.
#'
#' @return A data frame 
#' @export
#'
#' @examples
#' \dontrun{
#'  fit <- wqb_generate_ssd_fit(data)
#' }
wqb_generate_ssd_fit <- function(data) {
  ssdtools::ssd_fit_bcanz(
    data = data,
    left = "conc1_mean_std_effect_aggr"
  )
}


#' Get a SSD HC5
#'
#' @param fit The fit from ssd
#'
#' @return A data frame 
#'
#' @examples
#' \dontrun{
#'  fit <- wqb_generate_ssd_hc5(data)
#' }
wqb_generate_ssd_hc5 <- function(fit, nboot = 100) {
  ssdtools::ssd_hc_bcanz(fit, nboot = nboot)
}