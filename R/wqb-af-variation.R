#' Determine the Variation Assessment Factor
#'
#' Depending on the number of trophic groups and species a certain assessment 
#'  factor is determined.
#'
#' @param data A data frame that is the output of the `wqb_benchmark-method()`
#'   function.
#' @return A data frame
#' @export
#'
#' @examples
#' \dontrun{
#' data <- wqb_af_variation(data)
#' }
wqb_af_variation <- function(data) {
  chk::check_data(
    data, 
    list(
      trophic_group = "",
      species_number = 1L
    )
  ) 
  
  no_species <- data |> 
    dplyr::count(.data$species_number) |> 
    nrow()
  
  no_trophic_levels <- data |> 
    dplyr::count(.data$trophic_group) |> 
    nrow()
  ## Trophic Level 1 ----
  if (no_trophic_levels == 1){
    if (no_species == 1) {
      data$af_variation <- 50L
      return(data)
    }
    if (no_species %in% 2:3) {
      data$af_variation <- 20L
      return(data)
    }
    if (no_species %in% 4:6) { 
      data$af_variation <- 10L
      return(data)
    }
    if (no_species > 6) {
      data$af_variation <- 5L
      return(data)
    }
  }
  ## Trophic Level 2 ----
  if (no_trophic_levels == 2) {
    if (no_species == 1) {
      data$af_variation <- 50L
      return(data)
    }
    if (no_species %in% 2:3) {
      data$af_variation <- 10L
      return(data)
    }
    if (no_species %in% 4:6) { 
      data$af_variation <- 5L
      return(data)
    }
    if (no_species > 6) {
      data$af_variation <- 2L
      return(data)
    }
  }
  ## Trophic Level 3 ----
  if (no_trophic_levels == 3) {
    if (no_species == 1) {
      data$af_variation <- 50L
      return(data)
    }
    if (no_species %in% 2:3) {
      data$af_variation <- 5L
      return(data)
    }
    if (no_species %in% 4:6) { 
      data$af_variation <- 2L
      return(data)
    }
    if (no_species > 6) {
      data$af_variation <- 1L 
      return(data)
    }
  }
  data$af_variation <- 1L 
  data
}