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
wqb_plot <- function(data, y_axis = "effect_conc_std_mg.L") {
  chk::check_data(
    data, 
    list(
      cas = "",
      trophic_group = "",
      latin_name = ""
    )
  ) 
  
  data <- data |>
    dplyr::arrange(.data$trophic_group) |>
    dplyr::mutate(
      latin_name = factor(.data$latin_name, levels = unique(.data$latin_name)),
      species_present_in_bc = dplyr::if_else(.data$species_present_in_bc, "BC Species", "Other"),
      ecological_group = factor(.data$ecological_group, levels = c("Other", "Planktonic Invertebrate", "Salmonid"))
    )
  
  gp <- ggplot2::ggplot(data = data) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = .data[[y_axis]], 
        y = .data$latin_name,
        fill = .data$ecological_group,
        shape = .data$species_present_in_bc,
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
    ggplot2::scale_fill_manual(
      "Legend",
      values = c(
        "Other" = "#000000", 
        "Planktonic Invertebrate" = "#60C4EB", 
        "Salmonid" = "#E8613C"
      )
    ) +
    ggplot2::scale_shape_manual(
      "",
      values = c(
        "BC Species" = 24, 
        "Other" = 21
      ),
      breaks = c("BC Species")
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
    ggplot2::annotation_logticks(
      sides = "b",
      size = 0.3
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(order = 1, override.aes = list(shape = 21)),
      shape = ggplot2::guide_legend(order = 2)
    ) +
    NULL
  
  gp  
}