#' Determine the BC Species Assessment Factors
#' 
#' Depending on the number of BC species present a certain assessment 
#'  factor is determined.
#'
#' @param data A data frame that is the output of the `wqb_benchmark-method()`
#'   function.
#' @return A data frame
#' @export
#'
#' @examples
#' \dontrun{
#' data <- wqb_af_bc_species(data)
#' }
wqb_af_bc_species <- function(data) {
  chk::check_data(
    data, 
    list(
      species_present_in_bc = TRUE,
      species_number = 1L
    )
  ) 
  
  no_bc_species <- data |> 
    dplyr::filter(.data$species_present_in_bc) |> 
    dplyr::count(.data$species_number) |>
    nrow()
  
  if (no_bc_species <= 1) {
    data$af_bc_species <- 3
  }
  
  if (no_bc_species %in% 2:3) {
    data$af_bc_species <- 2
  }
  
  if (no_bc_species >= 4) {
    data$af_bc_species <- 1
  }
  
  data
}