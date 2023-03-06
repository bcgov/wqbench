#' Filter data set to a Single Chemical
#'
#' @param data A data frame 
#' @param cas_num A string of the cas number.
#'
#' @return Data frame
#' @export
#'
#' @examples
#' \dontrun{
#' data <- wqb_filter_chemical(data, "129909906")
#' data <- wqb_filter_chemical(data, "1000984359")
#' }
wqb_filter_chemical <- function(data, cas_num) {
  chk::check_data(
    data, 
    list(
      cas_number = ""
    )
  ) 
  
  data <- data |>
    dplyr::filter(.data$test_cas == cas_num)
  
  data
}


