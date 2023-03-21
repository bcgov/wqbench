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

#' Generate benchmark 
#' 
#' Pass the data to generate the benchmark for DET or SSD method. 
#'
#' @param data A data frame of clean and processed data filtered to only a 
#'  single chemical.
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' benchmark <- wqb_generate_bench(data)
#' }
wqb_generate_bench <- function(data) {
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
