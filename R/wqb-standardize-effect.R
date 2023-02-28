#' Standardize type of Effect
#'
#' @param data A data frame that is the output of the `wqb_classify_duration()` 
#'  function.
#' @return Data frame
#' @export
#'
#' @examples
wqb_standardize_effect <- function(data) {
  chk::check_data(
    data, 
    list(
    )
  ) 
  
  effect_std <- data |>
    dplyr::mutate()
  
}