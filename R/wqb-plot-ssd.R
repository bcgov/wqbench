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
    data, pred, shape = "ecological_group", color = "trophic_group", 
    label = "latin_name", left = "sp_aggre_conc_mg.L",
    xlab = "Concentration (mg/L)", ribbon = TRUE
  ) + 
    ggplot2::expand_limits(x = 3000) +
    ssdtools::scale_colour_ssd() +
    ggplot2::labs(color = "Trophic Group", shape = "Ecological Group")
  
  gp
}
