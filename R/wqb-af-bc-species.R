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

#' Determine the BC Species Assessment Factor
#' 
#' Determines an assessment factor based on the number of BC species present.
#'
#' @param data A data frame
#' @return A data frame
#' @export
#' @details Check the resource document for how the assessment was calculated.
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
    data$af_bc_species <- 3L
  }
  
  if (no_bc_species %in% 2:3) {
    data$af_bc_species <- 2L
  }
  
  if (no_bc_species >= 4) {
    data$af_bc_species <- 1L
  }
  
  data
}