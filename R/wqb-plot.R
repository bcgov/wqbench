#' Plots a single chemical 
#'
#' @param data A data frame of clean and processed data filtered to only a 
#'  single chemical.
#'
#' @return A ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' wqb_plot(data)
#' }
wqb_plot <- function(data, y_axis = "conc1_mean_std_effect") {
  chk::check_data(
    data, 
    list(
      test_cas = "",
      ecological_group = "",
      latin_name = ""
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
        x = .data[[y_axis]], 
        y = .data$latin_name,
        color = .data$ecological_group
      ),
      alpha = 0.8,
      size = 1.5
    ) +
    ggplot2::xlab("Concentration (mg/L)") +
    ggplot2::ylab("") +
    ggplot2::scale_color_manual(
      "Trophic Group",
      values = c(
        "#000000", "#3063A3", "#E8613C", "#821C65", "#63BB42", "#FFD446"
      )
    ) +
    ggplot2::theme_bw() + 
    ggplot2::theme(
      legend.position = "bottom",
      legend.background = ggplot2::element_rect(
        size = 0.2, linetype = "solid", colour = "black")
    ) +
    NULL
  
  gp 
}