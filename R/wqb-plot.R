#' Plots a single chemical 
#'
#' @param data A data frame of clean and processed data filtered to only a 
#'  single chemical.
#' @param y_axis A string of the column to use for the y-axis
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
        color = .data$ecological_group_class
      ),
      alpha = 0.8,
      size = 1.5
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data$ecological_group),
      scale = "free_y",
      space = "free_y"
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
        linetype = "solid", colour = "black")
    ) +
    ggplot2::scale_y_discrete(position = "right") +
    ggplot2::scale_x_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      labels = scales::label_comma()
    ) +
    ggplot2::annotation_logticks(
      sides = "b",
      size = 0.3
    ) +
    NULL
  
  gp 
}