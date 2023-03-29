# Copyright 2023 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at 
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' Determine the Species Variation Assessment Factor
#'
#'  Determine the assessment factor based on the number of trophic groups and
#'   species.
#'
#' @param data A data frame
#' @return A data frame
#' @export
#' @details Check the resource document for how the assessment factor was 
#'  calculated.
#'  
#' @examples
#' \dontrun{
#' data <- wqb_af_variation(data)
#' }
wqb_af_variation <- function(data) {
  chk::check_data(
    data, 
    list(
      trophic_group = factor(""),
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
  if (no_trophic_levels == 2) { ## not possible so remove
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