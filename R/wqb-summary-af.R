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

#' Summary Table of the Assessment Factor Values
#'
#' @param data A data frame
#' @return A data frame
#' @export
#'
#' @examples
#' \dontrun{
#' summary_af <- wqb_summary_af(data)
#' }
wqb_summary_af <- function(data) {
  chk::check_data(
    data, 
    list(
      af_variation = 1L,
      af_salmon = 1L,
      af_planktonic = 1L,
      af_bc_species = 1L
    )
  ) 
  
  af_descriptions <- data.frame(
    Consideration = c("af_variation", "af_ecological", "af_bc_species"),
    Description = c(
      "Accounts for uncertainty due to limited species and trophic coverage",
      "Accounts for uncertainty when missing data on planktonic invertebrates and/or salmonids",
      "Accounts for uncertainty of not having representation of B.C. species"
    )
  )
  
  tbl_af <- data |>
    dplyr::select(
      "af_variation", "af_salmon", "af_planktonic", "af_bc_species"
    ) |>
    dplyr::distinct() |>
    dplyr::mutate(
      af_ecological = .data$af_salmon * .data$af_planktonic
    ) |>
    tidyr::pivot_longer(
      cols = c("af_variation", "af_ecological", "af_bc_species"),
      names_to = "Consideration",
      values_to = "Assessment Factor"
    ) |>
    dplyr::left_join(af_descriptions, by = "Consideration") |>
    dplyr::mutate(
      Consideration = dplyr::case_when(
        stringr::str_detect(Consideration, "af_variation") ~ "Species variation factor",
        stringr::str_detect(Consideration, "af_ecological") ~ "Ecological assessment factor",
        stringr::str_detect(Consideration, "af_bc_species") ~ "B.C. species"
      )
    ) |>
    dplyr::select(
      "Consideration", "Assessment Factor", "Description"
    )
  
  tbl_af
}