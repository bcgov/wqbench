#' Plot the Deterministic Method Results
#'
#' @param data A data frame of the aggregated data with the assessment factors
#'
#' @return
#' @export
#'
#' @examples
wqb_plot_det <- function(data) {
  det <- wqb_generate_det(data)
  af <- wqb_summary_af(data)
  
  line_data <- tibble::tibble(
    names = c("Aquatic Life Benchmark", "Critical Toxicity Value"),
    values = c((det$ctv_est_mg.L/prod(af[['Assessment Factor']])), det$ctv_est_mg.L)
  )
  
  gp <- ggplot2::ggplot(data = data) +
    ggplot2::geom_vline(
      data = line_data,
      ggplot2::aes(
        xintercept = .data$values,
        linetype = .data$names,
        color = .data$names
      )
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        x = .data$"sp_aggre_conc_mg.L", 
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
    ggplot2::labs(caption = "The ecological group is shown with colour, the present of a species being from BC is shown with shape and the estimated values are shown with different linetype and colour. 
                The concentration values are on a log scale.") +
    ggplot2::scale_color_manual(
      "Value:",
      values = c(
        "Aquatic Life Benchmark" = "red", 
        "Critical Toxicity Value" = "black"
      )
    ) +
    ggplot2::scale_linetype_manual(
      "Value:",
      values = c(
        "Aquatic Life Benchmark" = "dashed", 
        "Critical Toxicity Value" = "solid"
      )
    ) +
    ggplot2::scale_fill_manual(
      "Ecological Group:",
      values = c(
        "Other" = "#000000", 
        "Planktonic Invertebrate" = "#60C4EB", 
        "Salmonid" = "#E8613C"
      )
    ) +
    ggplot2::scale_shape_manual(
      "BC Species:",
      values = c(
        "TRUE" = 24, 
        "FALSE" = 21
      )
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
    ggplot2::guides(
      fill = ggplot2::guide_legend(order = 1, override.aes = list(shape = 21)),
      shape = ggplot2::guide_legend(order = 2)
    ) +
    NULL
  
  gp
}
