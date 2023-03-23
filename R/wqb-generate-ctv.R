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

#' Generate Critical Toxicity Value
#' 
#' Determine the critical toxicity value for the species. The function will
#' apply the method listed in the method column. 
#'
#' @param data A data frame
#' @return A data frame
#' @export
#' @details The ctv_est_mg.L is the estimate of the critical toxicity value for 
#' the species. The ctv_lcl_mg.L is the lower confidence limit. The ctv_ucl_mg.L 
#' is the upper confidence limit.
#' 
#' The Deterministic method will always produce missing values in the 
#' ctv_lcl_mg.L and ctv_ucl_mg.L columns. 
#'
#' @examples
#' \dontrun{
#' ctv <- wqb_generate_ctv(data)
#' }
wqb_generate_ctv <- function(data) {
  chk::check_data(
    data,
    list(
      method = c("SSD", "Deterministic"),
      sp_aggre_conc_mg.L = 1
    )
  )
  
  method <- data$method[1]
  if (method == "Deterministic") {
    bench <- wqb_generate_det(data)
  } else {
    fit <- wqb_generate_ssd_fit(data)
    bench <- wqb_generate_ssd(data, fit)
  }
  bench
}
