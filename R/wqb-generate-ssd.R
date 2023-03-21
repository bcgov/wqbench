#' Generate the Benchmark Guideline Value using a SSD
#'
#' @param data A data frame of clean and processed data filtered to only a 
#'  single chemical.
#' @param fit The fit from ssd
#'
#' @return A data frame which contains the Critical Toxicity Value (HC5) and
#'  upper and lower bound. 
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
      sp_aggre_conc_mg.L = 1
    )
  )
  af <- gen_af_value(data)
  hc5 <- wqb_generate_ssd_hc5(fit)
  
  value <- hc5 |>
    dplyr::select("est", "lcl", "ucl") |>
    dplyr::mutate(
      ctv_est_mg.L = .data$est,
      ctv_lcl_mg.L = .data$lcl,
      ctv_ucl_mg.L = .data$ucl,
      .keep = "used"
    ) |>
    dplyr::select(
      "ctv_est_mg.L", "ctv_lcl_mg.L", "ctv_ucl_mg.L"
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
    left = "sp_aggre_conc_mg.L"
  )
}


#' Get a SSD HC5
#'
#' @param fit The fit from ssd
#' @param nboot A count of the number of bootstrap samples to use to estimate
#'   the se and confidence limits. A value of 10000 is recommended for official
#'   guidelines.
#'
#' @return A data frame
#'
#' @examples
#' \dontrun{
#'  fit <- wqb_generate_ssd_hc5(data)
#' }
wqb_generate_ssd_hc5 <- function(fit, nboot = 100) {
  ssdtools::ssd_hc_bcanz(fit, nboot = nboot) |>
    dplyr::filter(.data$percent == 5)
}
