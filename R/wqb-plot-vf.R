#' Title
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
wqb_plot_vf <- function(data) {
  vf <- wqb_generate_vf(data)
  
  gp <- wqb_plot(data, "conc1_mean_std_effect_aggr")
  gp <- gp +
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = vf$benchmark_value, linetype = "benchmark")
    ) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::scale_linetype_manual(
      "",
      values = c("dotted")
    )
  
  gp 
}