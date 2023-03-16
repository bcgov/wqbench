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
  
  col_name <- "conc1_mean_std_effect_aggr_mg.L"

  line_data <- tibble::tibble(
    names = c("Aquatic Life Benchmark", "Critical Toxicity Value"),
    values = c(det$benchmark_est, min(data[[col_name]]))
  )

  gp <- wqb_plot(data, col_name) +
    ggplot2::geom_vline(
      data = line_data,
      ggplot2::aes(
        xintercept = .data$values,
        linetype = .data$names,
        color = .data$names
      )
    ) +
    ggplot2::scale_color_manual(
      "",
      values = c(
        "Aquatic Life Benchmark" = "red", 
        "Critical Toxicity Value" = "black"
      )
    ) +
    ggplot2::scale_linetype_manual(
      "",
      values = c(
        "Aquatic Life Benchmark" = "dashed", 
        "Critical Toxicity Value" = "solid"
      )
    ) +
    NULL
  
  gp$layers <- c(gp$layers[[3]], gp$layers[[2]], gp$layers[[1]]) 
  
  gp 
}
