#' #' Determine the Ecological Assessment Factors
#'
#' @param data A data frame that is the output of the `wqb_benchmark-method()` function.
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
    data$salmon_af <- 2
  } else {
    data$salmon_af <- 1
  }
  
  if (no_ecological$planktonic_invertebrate == 0) {
    data$planktonic_af <- 2
  } else {
    data$planktonic_af <- 1
  }
  
  data
  
  
}