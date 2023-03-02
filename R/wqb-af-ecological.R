#' Determine the Ecological Assessment Factors
#'
#' Depending on the presence of certain ecological groups present in the data
#' set determines the assessment factor applied.
#'
#' @param data A data frame that is the output of the `wqb_benchmark-method()`
#'   function.
#' @return A data frame
#' @export
#'
#' @examples
#' \dontrun{
#' data <- wqb_af_ecological(data)
#' }
wqb_af_ecological <- function(data) {
  chk::check_data(
    data, 
    list(
      ecological_group_class = ""
    )
  ) 
  
  no_ecological <- data |> 
    dplyr::mutate(
      ecological_group_class = stringr::str_replace(.data$ecological_group_class, " ", "_"),
      ecological_group_class = stringr::str_to_lower(.data$ecological_group_class),
      ecological_group_class = factor(
        .data$ecological_group_class, 
        levels = c("planktonic_invertebrate", "regular", "salmonid")
      )
    ) |>
    dplyr::count(.data$ecological_group_class, .drop = FALSE) |> 
    tidyr::pivot_wider(
      names_from = "ecological_group_class",
      values_from = "n"
    )
  
  if (no_ecological$salmonid == 0) {
    data$af_salmon <- 2L
  } else {
    data$af_salmon <- 1L
  }
  
  if (no_ecological$planktonic_invertebrate == 0) {
    data$af_planktonic <- 2L
  } else {
    data$af_planktonic <- 1L
  }
  
  data
}