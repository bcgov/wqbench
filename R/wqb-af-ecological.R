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

#' Determine the Ecological Assessment Factors
#'
#' Determines the assessment factor based on the presence of certain ecological 
#' groups.
#'
#' @param data A data frame
#' @return A data frame
#' @export
#' @details Check the resource document for how the assessment was calculated.
#'
#' @examples
#' \dontrun{
#' data <- wqb_af_ecological(data)
#' }
wqb_af_ecological <- function(data) {
  chk::check_data(
    data, 
    list(
      ecological_group = factor("")
    )
  ) 
  
  no_ecological <- data |> 
    dplyr::mutate(
      ecological_group = stringr::str_replace(.data$ecological_group, " ", "_"),
      ecological_group = stringr::str_to_lower(.data$ecological_group),
      ecological_group = factor(
        .data$ecological_group,
        levels = c("planktonic_invertebrate", "other", "salmonid")
      )
    ) |>
    dplyr::count(.data$ecological_group, .drop = FALSE) |> 
    tidyr::pivot_wider(
      names_from = "ecological_group",
      values_from = "n"
    )
  
  if (no_ecological$salmonid == 0) {
    data$af_salmon <- 2L
  } else {
    data$af_salmon <- 1L
  }
  
  if (no_ecological$planktonic_invertebrate == 0) {
    data$af_planktonic <- 2L
  } else {
    data$af_planktonic <- 1L
  }
  
  data
}