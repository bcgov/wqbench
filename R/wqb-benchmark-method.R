#' Determine Benchmark Method
#' 
#' Add a column called `method` which either will say SSD or AF.
#' 
#' @param data A data frame that is the output of the `wqb_aggregate()` function.
#' @return A data frame
#' @export
#' @details The data is run through a set of rules to determine if a species 
#' sensitivity distribution (SSD) or deterministic (DET) method can be used on 
#' the data set. 
#' 
#' Conditions for SSD:
#'  - number of fish species ≥ 3 AND 
#'  - number of invertebrates ≥ 3 AND 
#'  - number of plants or algae species ≥ 1
#'  
#' If these conditions are not met then a variation factor is the required. 
#'
#' @examples
#' \dontrun{
#' data <- wqb_benchmark_method(data)
#' data <- wqb_benchmark_method(aggregated_data)
#' }
wqb_benchmark_method <- function(data) {
  chk::check_data(
    data, 
    list(
      trophic_group = factor("")
    )
  ) 
  
  groups_species <- 
    data |>
    dplyr::mutate(
      trophic_group = stringr::str_to_lower(.data$trophic_group),
      trophic_group = factor(
        .data$trophic_group, 
        levels = c("fish", "amphibian", "invertebrate", "algae", "plant")
      )
    ) |>
    dplyr::count(.data$trophic_group, .drop = FALSE) |>
    tidyr::pivot_wider(
      names_from = "trophic_group",
      values_from = "n"
    )
  
  if (groups_species$fish >= 3 & groups_species$invertebrate >= 3 & (groups_species$algae >=1 | groups_species$plant >= 1)) {
    data$method <- "SSD"
  } else {
    data$method <- "Deterministic" 
  }
  
  data
}