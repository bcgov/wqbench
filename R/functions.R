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

geometric_mean <- function(x) {
  exp(mean(log(x)))
}

gen_af_table <- function(data) {
  chk::check_data(
    data,
    list(
      af_variation = 1L, 
      af_salmon = 1L, 
      af_planktonic = 1L, 
      af_bc_species = 1L
    )
  )
  
  af_table <- data |>
    dplyr::select(
      "af_variation", "af_salmon", "af_planktonic", "af_bc_species"
    ) |>
    dplyr::distinct() |>
    tidyr::pivot_longer(
      cols = c("af_variation", "af_salmon", "af_planktonic", "af_bc_species"),
      names_to = "name",
      values_to = "value"
    ) 
  af_table
}

gen_af_value <- function(data) {
  af_table <- gen_af_table(data)
  af <- prod(af_table$value)
  af
}