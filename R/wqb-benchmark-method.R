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

#' Determine Benchmark Method
#'
#' Determine if a species sensitivity distribution (SSD) or deterministic method
#' will be used to determine the aquatic life water quality benchmark value
#' for the data. A column called `method` will be added to the data indicating
#' which method will be used.
#'
#' @param data A data frame
#' @return A data frame
#' @export
#' @details Check the resource document for the rules used to determine which
#' benchmark method will be used. This is Step 4.
#'
#' @examples
#' \dontrun{
#' data <- wqb_benchmark_method(data)
#' data <- wqb_benchmark_method(aggregated_data)
#' }
wqb_benchmark_method <- function(data) {
  chk::check_data(
    data,
    list(
      trophic_group = factor("")
    )
  )

  groups_species <-
    data |>
    dplyr::mutate(
      trophic_group = stringr::str_to_lower(.data$trophic_group),
      trophic_group = factor(
        .data$trophic_group,
        levels = c("fish", "amphibian", "invertebrate", "algae", "plant")
      )
    ) |>
    dplyr::count(.data$trophic_group, .drop = FALSE) |>
    tidyr::pivot_wider(
      names_from = "trophic_group",
      values_from = "n"
    )

  if (groups_species$fish >= 3 & groups_species$invertebrate >= 3 & (groups_species$algae >= 1 | groups_species$plant >= 1)) {
    data$method <- "SSD"
  } else {
    data$method <- "Deterministic"
  }

  data
}
