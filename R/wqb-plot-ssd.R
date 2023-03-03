#' Plot SSD
#'
#' @param data A data frame of clean and processed data filtered to only a 
#'  single chemical.
#' @param fit The fit from ssd
#'
#' @return
#' @export
#'
#' @examples
wqb_plot_ssd <- function(data, fit) {
  
  pred <- ssdtools::predict(fit, ci = TRUE)
  gp <- ssdtools::ssd_plot(
    data, pred, shape = "ecological_group", color = "ecological_group", 
    label = "common_name", left = "conc1_mean_std_effect_aggr",
    xlab = "Concentration (mg/L)", ribbon = TRUE
  ) + 
    ggplot2::expand_limits(x = 3000) +
    ssdtools::scale_colour_ssd()
  
  gp
}