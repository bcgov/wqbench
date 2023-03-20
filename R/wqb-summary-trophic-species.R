#' Summary Table of Number of Species per Trophic Group
#'
#' @param data A data frame
#' @return A data frame.
#' @export
#'
#' @examples
#' \dontrun{
#' summary_trophic <- wqb_summary_trophic_species(data)
#' }
wqb_summary_trophic_species <- function(data) {
  chk::check_data(
    data, 
    list(
      trophic_group = factor("")
    )
  ) 
  
  data |>
    dplyr::count(.data$trophic_group, .drop = FALSE) |> 
    dplyr::rename(
      "Trophic Group" = "trophic_group",
      "Number of Species" = "n"
    )
  
}