#' Title
#'
#' @param data A dataframe of the aggregated data with the assessment factors
#'
#' @return
#' @export
#'
#' @examples
wqb_plot_det <- function(data) {
  det <- wqb_generate_det(data)
  
  gp <- wqb_plot(data, "conc1_mean_std_effect_aggr")
  gp <- gp +
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = det$benchmark_est, linetype = "benchmark")
    ) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::scale_linetype_manual(
      "",
      values = c("dotted")
    )
  
  gp 
}