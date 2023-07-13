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

#' Summary Table of Assessment Factors Information
#'
#' @param data A data frame
#' @return A data frame
#' @export
#'
#' @examples
#' \dontrun{
#' summary_af <- wqb_summary_trophic_groups(data)
#' }
wqb_summary_trophic_groups <- function(data) {
  chk::check_data(
    data,
    list(
      ecological_group = factor(""),
      trophic_group = factor(""),
      species_present_in_bc = TRUE,
      latin_name = ""
    )
  )

  bc_species_names <-
    data |>
    dplyr::filter(.data$species_present_in_bc) |>
    dplyr::select("latin_name") |>
    dplyr::pull()

  trophic_group_names <-
    data |>
    dplyr::count(.data$trophic_group) |>
    dplyr::select("trophic_group") |>
    dplyr::pull()

  summary_tbl <-
    data |>
    dplyr::count(.data$ecological_group, .drop = FALSE) |>
    dplyr::filter(.data$ecological_group != "Other") |>
    dplyr::mutate(
      Result = dplyr::case_when(
        .data$ecological_group == "Planktonic Invertebrate" & .data$n != 0 ~ paste(sort(unique(data$latin_name[data$ecological_group == "Planktonic Invertebrate"])), collapse = ", "),
        .data$ecological_group == "Salmonid" & .data$n != 0 ~ paste(sort(unique(data$latin_name[data$ecological_group == "Salmonid"])), collapse = ", "),
        TRUE ~ "None"
      )
    ) |>
    dplyr::select(
      Consideration = "ecological_group",
      "Result"
    ) |>
    dplyr::bind_rows(
      tibble::tibble(
        Consideration = "B.C. species",
        Result = paste(sort(unique(bc_species_names)), collapse = ", ")
      )
    ) |>
    dplyr::mutate(
      Result = dplyr::if_else(
        .data$Consideration == "B.C. species" & stringr::str_detect(.data$Result, "$^"),
        "None",
        .data$Result
      )
    ) |>
    dplyr::bind_rows(
      tibble::tibble(
        Consideration = "Trophic group(s)",
        Result = paste(sort(unique(trophic_group_names)), collapse = ", ")
      )
    ) |>
    dplyr::arrange(dplyr::desc(.data$Consideration)) |>
    dplyr::mutate(
      Consideration = dplyr::if_else(.data$Consideration == "Salmonid", "Salmonid(s)", .data$Consideration),
      Consideration = dplyr::if_else(.data$Consideration == "Planktonic Invertebrates", "Planktonic Invertebrate(s)", .data$Consideration)
    )

  summary_tbl
}
