#' Generate the Benchmark Guideline Value using a SSD
#'
#' @param data 
#'
#' @return A data frame which contains the HC5 value, the total 
#'  assessment factor and the benchmark value.
#' @export
#'
#' @examples
#' \dontrun{
#'  bench_iodine <- wqb_generate_ssd(data)
#' }
wqb_generate_ssd <- function(data) {
  chk::check_data(
    data,
    list(
      method = c("SSD", "SSD", "SSD"),
      conc1_mean_std_effect_aggr = 1
    )
  )
  af <- gen_af_value(data)
  
  fit <- wqb_generate_ssd_fit(data)
  hc5 <- wqb_generate_ssd_hc5(fit)
  
  value <- hc5 |>
    dplyr::select(
      est, lcl, ucl
    ) |>
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
#' @param data 
#'
#' @return A data frame 
#'
#' @examples
#' \dontrun{
#'  fit <- wqb_generate_ssd_fit(data)
#' }
wqb_generate_ssd_fit <- function(data) {
  fit <- ssdtools::ssd_fit_dists(
    data = data,
    left = "conc1_mean_std_effect_aggr"
  )
  fit
}


#' Get a SSD HC5
#'
#' @param data 
#'
#' @return A data frame 
#'
#' @examples
#' \dontrun{
#'  fit <- wqb_generate_ssd_fit(data)
#' }
wqb_generate_ssd_hc5 <- function(fit) {
  hc5 <- ssdtools::ssd_hc(fit, ci = TRUE, nboot = 100, percent = 5)
}