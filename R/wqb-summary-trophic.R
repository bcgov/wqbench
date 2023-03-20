#' Summary Table of Assessment Factors Information
#'
#' @param data A data frame
#' @return A data frame
#' @export
#'
#' @examples
#' \dontrun{
#' summary_af <- wqb_summary_trophic_groups(data)
#' }
wqb_summary_trophic_groups <- function(data) {
  chk::check_data(
    data, 
    list(
      ecological_group = factor(""),
      trophic_group = factor(""),
      species_present_in_bc = TRUE
    )
  ) 
  
  bc_species_names <- 
    data |>
    dplyr::filter(.data$species_present_in_bc) |>
    dplyr::select("latin_name") |>
    dplyr::pull()
  
  trophic_group_names <- 
    data |>
    dplyr::count(.data$trophic_group) |>
    dplyr::select("trophic_group") |>
    dplyr::pull()
  
  summary_tbl <- 
    data |>
    dplyr::count(.data$ecological_group, .drop = FALSE) |>
    dplyr::filter(.data$ecological_group != "Other") |>
    dplyr::mutate(
      Result = dplyr::case_when(
        .data$ecological_group == "Planktonic Invertebrate" & .data$n != 0 ~ paste(sort(unique(data$latin_name[data$ecological_group == "Planktonic Invertebrate"])), collapse = ", "),
        .data$ecological_group == "Salmonid" & .data$n != 0 ~ paste(sort(unique(data$latin_name[data$ecological_group == "Salmonid"])), collapse = ", "),
        TRUE ~ "None"
      )
    ) |>
    dplyr::select(
      Consideration = "ecological_group",
      "Result"
    ) |>
    dplyr::bind_rows(
      tibble::tibble(
        Consideration = "B.C. species",
        Result = paste(sort(unique(bc_species_names)), collapse = ", ")
      )
    ) |>
    dplyr::mutate(
      Result = dplyr::if_else(
        .data$Consideration == "B.C. species" & stringr::str_detect(.data$Result, "$^"),
        "None",
        .data$Result
      )
    ) |>
    dplyr::bind_rows(
      tibble::tibble(
        Consideration = "Trophic group(s)",
        Result = paste(sort(unique(trophic_group_names)), collapse = ", ")
      )
    ) |>
    dplyr::arrange(dplyr::desc(.data$Consideration))
  
  
  summary_tbl
}