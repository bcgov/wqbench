#' Generate benchmark 
#' 
#' Pass the data to generate the benchmark for DET or SSD method. 
#'
#' @param data A data frame of clean and processed data filtered to only a 
#'  single chemical.
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' benchmark <- wqb_generate_bench(data)
#' }
wqb_generate_bench <- function(data) {
  chk::check_data(
    data,
    list(
      method = c("SSD", "Deterministic"),
      conc1_mean_std_effect_aggr_mg.L = 1
    )
  )
  
  method <- data$method[1]
  if (method == "Deterministic") {
    bench <- wqb_generate_det(data)
  } else {
    fit <- wqb_generate_ssd_fit(data)
    bench <- wqb_generate_ssd(data, fit)
  }
  bench
}