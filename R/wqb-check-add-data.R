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

#' Check the Uploaded Data
#'
#' Checks the uploaded data for the basic requirements to ensure the data
#' matches the downloaded Ecotox data.
#'
#' @param data A data frame. The data you want to check.
#' @param template A data frame. The format the data should be in, in the
#'   [chktemplate](https://poissonconsulting.github.io/chktemplate/) format.
#'
#' @return A data frame
#' @export
#' @details The values for the endpoint, trophic_group, and ecological_group
#'   columns are checked against the data tables used to build the database. To
#'   update the allowed values the corresponding csv file needs to be updated.
#'
#' @examples
#' \dontrun{
#' data <- wqb_check_add_data(data, template)
#' }
wqb_check_add_data <- function(data, template) {
  data <- chktemplate::check_data_format(
    data = data, template = list(data = template)
  )
  data <- data$data

  data <- check_no_extra_cols(data, template)

  data <- data |>
    dplyr::mutate(
      simple_lifestage = stringr::str_to_lower(.data$simple_lifestage),
      endpoint = stringr::str_to_upper(.data$endpoint),
      trophic_group = stringr::str_to_sentence(.data$trophic_group),
      ecological_group = stringr::str_to_title(.data$ecological_group) 
    )

  check_simple_lifestage(data)
  check_endpoint(data)
  check_trophic_eco_group(data)
  check_species_present(data)
  data$species_present_in_bc <- as.logical(data$species_present_in_bc)

  data
}

check_simple_lifestage <- function(data) {
  lifestage_fp <- system.file(
    "extdata/lifestage-codes.csv",
    package = "wqbench"
  )
  lifestage <- readr::read_csv(
    lifestage_fp,
    show_col_types = FALSE
  ) |>
    dplyr::select("simple_lifestage") |>
    dplyr::distinct() |>
    dplyr::mutate(simple_lifestage = stringr::str_to_lower(.data$simple_lifestage))

  if (!all(data$simple_lifestage %in% lifestage$simple_lifestage)) {
    chk::abort_chk(
      "The simple_lifestage column has invalid value(s). The allowed values include: ",
      paste(lifestage$simple_lifestage, collapse = ", ")
    )
  }
}

check_endpoint <- function(data) {
  endpoints_fp <- system.file(
    "extdata/concentration-endpoints.csv",
    package = "wqbench"
  )
  endpoints <- readr::read_csv(
    endpoints_fp,
    show_col_types = FALSE
  ) |>
    dplyr::select("code") |>
    dplyr::filter(!stringr::str_detect(.data$code, "log")) |>
    dplyr::filter(!stringr::str_detect(.data$code, "\\*")) |>
    dplyr::distinct()

  if (!all(data$endpoint %in% endpoints$code)) {
    chk::abort_chk(
      "The endpoint column has invalid value(s). The allowed values include: ",
      paste(endpoints$code, collapse = ", ")
    )
  }
}

check_trophic_eco_group <- function(data) {
  trophic_groups_fp <- system.file(
    "extdata/trophic-group.csv",
    package = "wqbench"
  )
  trophic_eco_groups <- readr::read_csv(
    trophic_groups_fp,
    show_col_types = FALSE
  ) |>
    dplyr::select("trophic_group", "ecological_group") |>
    dplyr::distinct()
  
  if (!all(data$trophic_group %in% trophic_eco_groups$trophic_group)) {
    chk::abort_chk(
      "The trophic_group column has invalid value(s). The allowed values include: ",
      paste(sort(unique(trophic_eco_groups$trophic_group)), collapse = ", ")
    )
  }

  if (!all(data$ecological_group %in% trophic_eco_groups$ecological_group)) {
    chk::abort_chk(
      "The ecological_group column has invalid value(s). The allowed values include: ",
      paste(sort(unique(trophic_eco_groups$ecological_group)), collapse = ", ")
    )
  }

  if (!chk::vld_join(data, trophic_eco_groups, by = c("trophic_group", "ecological_group"))) {
    allowed_vals <- trophic_eco_groups |>
      dplyr::arrange(.data$trophic_group, .data$ecological_group) |>
      dplyr::mutate(
        trophic_eco_group = paste(
          .data$trophic_group,
          .data$ecological_group,
          sep = " & "
        )
      ) |>
      dplyr::pull("trophic_eco_group")
    chk::abort_chk(
      "There is an invalid combination of the trophic_group or ",
      "ecological_group columns. The allowed values include: ",
      paste(allowed_vals, collapse = ", ")
    )
  }
}

check_species_present <- function(data) {
  if (!all(data$species_present_in_bc %in% c("TRUE", "FALSE"))) {
    chk::abort_chk(
      "The species_present_in_bc column has invalid value(s). ",
      "The allowed values include: TRUE or FALSE",
    )
  }
}

check_no_extra_cols <- function(data, template) {
  data <- data |>
    dplyr::select(dplyr::all_of(names(template)[-1]))
  data
}
