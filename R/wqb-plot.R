#' Plots a single chemical 
#'
#' @param data A data frame of clean and processed data filtered to only a 
#'  single chemical.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' wqb_plot(data)
#' }
wqb_plot <- function(data) {
  chk::check_data(
    data, 
    list(
      test_cas = "",
      ecological_group = "",
      latin_name = "",
      conc1_mean_std_effect = 1
    )
  ) 
  
  data <- data |>
    dplyr::arrange(.data$ecological_group) |>
    dplyr::mutate(
      latin_name = factor(.data$latin_name, levels = unique(.data$latin_name))
    )
  
  gp <- ggplot2::ggplot(data = data) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = .data$conc1_mean_std_effect, 
        y = .data$latin_name,
        color = .data$ecological_group
      ),
      alpha = 0.6
    ) +
    ggplot2::xlab("Concentration (mg/L)") +
    ggplot2::ylab("") +
    ggplot2::labs(color = "Trophic Group") +
    ggplot2::theme_bw() + 
    ggplot2::theme(
      legend.position = "bottom",
      legend.background = ggplot2::element_rect(
        size = 0.2, linetype = "solid", colour = "black")
    ) +
    NULL
  
  gp 
}