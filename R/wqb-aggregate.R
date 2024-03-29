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

#' Aggregate Data for each Species
#'
#' Aggregate data to select the most sensitive value for each species. The
#' priority of effect level depends on the benchmark method that will be used.
#'
#' @param data A data frame
#' @return A data frame
#' @export
#' @details Check the resource document for the rules used to aggregate the data
#'  down to a single value per species. This is Step 5.
#' @examples
#' \dontrun{
#' data <- wqb_aggregate(data)
#' }
wqb_aggregate <- function(data) {
  chk::check_data(
    data,
    list(
      species_number = 1L,
      lifestage = "",
      effect = "",
      effect_conc_std_mg.L = 1,
      method = ""
    )
  )

  method <- unique(data$method)

  if (length(method) != 1) {
    stop("Only 1 method allowed per data set")
  }

  if (method == "SSD") {
    grouped_data <- data |>
      dplyr::group_by(.data$species_number, .data$lifestage, .data$effect) |>
      dplyr::mutate(
        endpoint_alpha = stringr::str_extract(.data$endpoint, "[:alpha:]+"),
        endpoint_numeric = stringr::str_extract(.data$endpoint, "[:digit:]+"),
        effect_priority_order = dplyr::case_when(
          stringr::str_detect(.data$endpoint_alpha, "EC|IC") & .data$endpoint_numeric <= 10 ~ 8,
          stringr::str_detect(.data$endpoint_alpha, "EC|IC") & .data$endpoint_numeric >= 11 & .data$endpoint_numeric <= 20 ~ 7,
          stringr::str_detect(.data$endpoint_alpha, "MATC") ~ 6,
          stringr::str_detect(.data$endpoint_alpha, "(NOEC)|(NOEL)") ~ 5,
          stringr::str_detect(.data$endpoint_alpha, "(LOEC)|(LOEL)|(MCIG)") ~ 4,
          stringr::str_detect(.data$endpoint_alpha, "EC|IC") & .data$endpoint_numeric > 20 ~ 3,
          stringr::str_detect(.data$endpoint_alpha, "LC") & .data$endpoint_numeric < 20 ~ 2,
          stringr::str_detect(.data$endpoint_alpha, "LC") & .data$endpoint_numeric >= 20 ~ 1
        ),
        group_id = dplyr::cur_group_id(),
        id = seq_len(dplyr::n())
      )
  } else {
    # Deterministic method
    grouped_data <- data |>
      dplyr::group_by(.data$species_number, .data$lifestage, .data$effect) |>
      dplyr::mutate(
        endpoint_alpha = stringr::str_extract(.data$endpoint, "[:alpha:]+"),
        endpoint_numeric = stringr::str_extract(.data$endpoint, "[:digit:]+"),
        effect_priority_order = dplyr::case_when(
          stringr::str_detect(.data$endpoint_alpha, "EC|IC") & .data$endpoint_numeric <= 10 ~ 8,
          stringr::str_detect(.data$endpoint_alpha, "EC|IC") & .data$endpoint_numeric >= 11 & .data$endpoint_numeric <= 20 ~ 7,
          stringr::str_detect(.data$endpoint_alpha, "MATC") ~ 6,
          stringr::str_detect(.data$endpoint_alpha, "(LOEC)|(LOEL)|(MCIG)") ~ 5,
          stringr::str_detect(.data$endpoint_alpha, "EC|IC") & .data$endpoint_numeric > 20 ~ 4,
          stringr::str_detect(.data$endpoint_alpha, "LC") & .data$endpoint_numeric < 20 ~ 3,
          stringr::str_detect(.data$endpoint_alpha, "LC") & .data$endpoint_numeric >= 20 ~ 2,
          stringr::str_detect(.data$endpoint_alpha, "(NOEC)|(NOEL)") ~ 1,
        ),
        group_id = dplyr::cur_group_id(),
        id = seq_len(dplyr::n())
      )
  }

  aggregated_data <-
    grouped_data |>
    dplyr::filter(.data$effect_priority_order == max(.data$effect_priority_order)) |>
    dplyr::mutate(
      sp_aggre_conc_mg.L = geometric_mean(.data$effect_conc_std_mg.L)
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$species_number) |>
    dplyr::arrange(.data$sp_aggre_conc_mg.L) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::arrange(.data$species_number) |>
    dplyr::select(
      "chemical_name",
      "cas",
      "latin_name",
      "common_name",
      "effect",
      "sp_aggre_conc_mg.L",
      "trophic_group",
      "ecological_group",
      "species_present_in_bc",
      "method",
      "species_number"
    )

  aggregated_data
}
