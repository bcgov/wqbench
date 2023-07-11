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

#' Determine the Assessment Factors
#'
#' Determines the species variation factor, ecological assessment factor and
#' B.C. species factor.
#'
#' @param data A data frame
#' @return A data frame
#' @export
#' @details This is a wrapper function that calls `wqb_af_bc_species()`,
#'  `wqb_af_ecological()` and `wqb_af_variation()`.
#'
#' @examples
#' \dontrun{
#' data <- wqb_af(data)
#' }
wqb_af <- function(data) {
  data <- wqb_af_bc_species(data)
  data <- wqb_af_ecological(data)
  data <- wqb_af_variation(data)
  data
}
