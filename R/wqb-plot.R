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
wqb_plot <- function(data) {
  chk::check_data(
    data, 
    list(
      latin_name = "",
      effect_conc_std_mg.L = 1,
      endpoint = "",
      trophic_group = factor("")
    )
  ) 
  
  data <- data |>
    dplyr::arrange(.data$trophic_group) |>
    dplyr::mutate(
      latin_name = factor(.data$latin_name, levels = sort(unique(.data$latin_name))),
    )
  
  gp <- ggplot2::ggplot(data = data) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = .data$effect_conc_std_mg.L, 
        y = .data$latin_name,
        shape = .data$endpoint,
      ),
      alpha = 0.8,
      size = 1.5
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data$trophic_group),
      scale = "free_y",
      space = "free_y"
    ) +
    ggplot2::xlab("Concentration (mg/L)") +
    ggplot2::ylab("") +
    ggplot2::labs(
      shape = "Endpoint",
      caption = "The concentration values are on a log scale."
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "bottom",
      panel.border = ggplot2::element_rect(colour = "black", fill = NA),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_rect(colour = "black"),
      strip.text.y = ggplot2::element_text(angle = 0)
    ) +
    ggplot2::scale_y_discrete(
      position = "right", 
      expand = ggplot2::expansion(add = 0.5)
    ) +
    ggplot2::scale_x_log10(
      breaks = scales::trans_breaks("log10", function(x) 10^x),
      label = ~ ifelse(
        .x < 10, 
        sprintf("%g", signif(.x, 3)), 
        scales::comma(.x, accuracy = 1)
      )
    ) +
    NULL
  
  gp  
}