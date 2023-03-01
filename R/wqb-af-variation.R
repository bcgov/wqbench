#' Determine the Assessment Factor
#' 
#' Based on a set of rules determine all the assessment factors for the 
#' data set. 
#'
#' @param data A data frame that is the output of the `wqb_benchmark-method()` function.
#' @return A data frame
#' @export
#'
#' @examples
wqb_af_variation <- function(data) {
  chk::check_data(
    data, 
    list(
      ecological_group = "",
      species_number = 1L
    )
  ) 
  
  no_species <- data |> 
    dplyr::count(.data$species_number) |> 
    nrow()
  
  no_trophic_levels <- data |> 
    dplyr::count(.data$ecological_group) |> 
    nrow()
  ## Trophic Level 1 ----
  if (no_trophic_levels == 1){
    if (no_species == 1) {
      data$variation_factor <- 50L
      return(data)
    }
    if ((no_species == 2 | no_species == 3)) {
      data$variation_factor <- 20L
      return(data)
    }
    if ((no_species == 4 | no_species == 6)) {
      data$variation_factor <- 10L
      return(data)
    }
    if (no_species >= 6) {
      data$variation_factor <- 5L
      return(data)
    }
  }
  ## Trophic Level 2 ----
  if (no_trophic_levels == 2) {
    if (no_species == 1) {
      data$variation_factor <- NA_integer_
      return(data)
    }
    if (no_species == 2 | no_species == 3) {
      data$variation_factor <- 10L
      return(data)
    }
    if (no_species == 4 | no_species == 6) {
      data$variation_factor <- 50L
      return(data)
    }
    if (no_species >= 6) {
      data$variation_factor <- 2L
      return(data)
    }
  }
  ## Trophic Level 3 ----
  if (no_trophic_levels == 3) {
    if (no_species == 1) {
      data$variation_factor <- NA_integer_
      return(data)
    }
    if (no_species == 2 | no_species == 3) {
      data$variation_factor <- 5L
      return(data)
    }
    if (no_species == 4 | no_species == 6) {
      data$variation_factor <- 2L
      return(data)
    }
    if (no_species >= 6) {
      data$variation_factor <- 1L 
      return(data)
    }
  }
  data$variation_factor <- 1L 
  data
}