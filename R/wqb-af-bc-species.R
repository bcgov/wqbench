#' Determine the BC Species Assessment Factors
#'
#' @param data A data frame that is the output of the `wqb_benchmark-method()` function.
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
  
  data_af <- data |>
    dplyr::mutate(
      af_bc_species = dplyr::case_when(
        no_bc_species > 4 ~ 1,
        no_bc_species == 2 | no_bc_species == 3 ~ 2,
        no_bc_species <= 1 ~ 3
      )
    )
  data_af
}