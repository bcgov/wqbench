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

#' SSD Method to Calculate Critical Toxicity Value for the Chemical
#'
#' Use a species sensitivity distribution to calculate the critical toxicity
#' value for the data set. The critical toxicity value is the hazardous
#' concentration for 5% of species (HC5) when the SSD method is used.
#'
#' @param data A data frame
#' @param fit The fit
#' @param nboot The number of bootstrap samples. Default value of 1000.
#' @return A data frame
#' @export
#' @details A wrapper on the ssdtools package function
#'   [ssdtools::ssd_hc_bcanz()] that only returns the HC5 concentration.
#'
#' @examples
#' \dontrun{
#' hc5 <- wqb_method_ssd(data, fit)
#' }
wqb_method_ssd <- function(data, fit, nboot = 1000) {
  chk::check_data(
    data,
    list(
      method = c("SSD", "SSD", "SSD"),
      sp_aggre_conc_mg.L = 1
    )
  )
  hc5 <- wqb_ssd_hc5(fit, nboot = nboot)

  value <- hc5 |>
    dplyr::select("est", "lcl", "ucl") |>
    dplyr::mutate(
      ctv_est_mg.L = .data$est,
      ctv_lcl_mg.L = .data$lcl,
      ctv_ucl_mg.L = .data$ucl,
      .keep = "used"
    ) |>
    dplyr::select(
      "ctv_est_mg.L", "ctv_lcl_mg.L", "ctv_ucl_mg.L"
    )

  value
}

#' Fit BCANZ Distributions
#'
#' Wrapper to [ssdtools::ssd_fit_bcanz()]. The sp_aggre_conc_mg.L values are
#' the concentrations used.
#'
#' @param data A data frame
#' @param dists A character vector of the distributions to fit
#' @return A data frame
#' @export
#'
#' @examples
#' \dontrun{
#' fit <- wqb_ssd_fit(data)
#' }
wqb_ssd_fit <- function(data, dists = ssdtools::ssd_dists_bcanz()) {
  ssdtools::ssd_fit_bcanz(
    data = data,
    left = "sp_aggre_conc_mg.L",
    dists = dists
  )
}

#' Run SSD to get Hazard Concentrations
#'
#' Wrapper to the [ssdtools::ssd_hc_bcanz()] function and selects only the row
#' which is 5%.
#'
#' @param fit The fit from ssd
#' @param nboot A count of the number of bootstrap samples to use to estimate
#'   the SE and confidence limits. Default value of 1000.
#' @return A data frame
#' @details The number of bootstrap samples is set to 1000 so the estimates are
#'   quick to generate. Check out [ssdtools::ssd_hc_bcanz()] for more details.
#'
#' @examples
#' \dontrun{
#' hc5 <- wqb_ssd_hc5(fit)
#' }
wqb_ssd_hc5 <- function(fit, nboot = 1000) {
  tbl <- ssdtools::ssd_hc_bcanz(fit, nboot = nboot) |>
    dplyr::filter(.data$proportion == 0.05)
  tbl
}
