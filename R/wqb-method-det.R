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

#' Deterministic Method to Generate Critical Toxicity Value for the Chemical
#'
#' Use the deterministic method to generate the critical toxicity value for the 
#' data set.
#'
#' @param data A data frame 
#' @return A data frame
#' @export
#' @details Check the resource document for more details . This is Step 1. 
#'
#' @examples
#' \dontrun{
#' bench_iodine <- wqb_method_det(data)
#' }
wqb_method_det <- function(data) {
  chk::check_data(
    data,
    list(
      method = c("Deterministic", "Deterministic", "Deterministic"),
      sp_aggre_conc_mg.L = 1
    )
  )
  value <- data |>
    dplyr::arrange(.data$sp_aggre_conc_mg.L) |>
    dplyr::slice(1) |>
    dplyr::select("sp_aggre_conc_mg.L") |>
    dplyr::mutate(
      ctv_est_mg.L = .data$sp_aggre_conc_mg.L,
      ctv_lcl_mg.L = NA_real_,
      ctv_ucl_mg.L = NA_real_
    ) |>
    dplyr::select(
      "ctv_est_mg.L", "ctv_lcl_mg.L", "ctv_ucl_mg.L"
    )
  value 
}
