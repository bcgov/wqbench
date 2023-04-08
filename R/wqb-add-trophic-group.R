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

#' Add Trophic Groups to Database
#' 
#' Read in the trophic and ecological groups and and add a column to the 
#' `species` table in the database that lists the groups for each species. 
#'
#' @param database A string to the location of the database.
#' @param quiet Turn off message when quiet set to TRUE.
#' @return Invisible data frame
#' @export
#' @details The trophic group data is contained in a csv file in the extdata 
#' folder of the package. This file can be edited by adding or removing groups 
#' and classes. Do not add new columns, rename columns or rename the file.
#' 
#' The trophic groups file must contain the columns: `class`, `order`, 
#' `trophic_group`, and `ecological_group`. The `class` and `order`
#' columns are matched to the `class` and `tax_order` columns in the `species`
#' table of the database and then adds the `trophic_group` and 
#' `ecological_group` columns to the species table. 
#' 
#' @examples
#' \dontrun{
#' trophic_group <- wqb_add_trophic_group(
#'  database = "ecotox_ascii_09_15_2022.sqlite"
#' ) 
#' 
#' trophic_group <- wqb_add_trophic_group(
#'  database = "ecotox_db/ecotox_ascii_09_15_2022.sqlite"
#' ) 
#' }
wqb_add_trophic_group <- function(database, quiet = FALSE) {
  
  chk::chk_file(database)
  chk::chk_ext(database, "sqlite")
  
  # read in species from db
  on.exit(DBI::dbDisconnect(con))
  con  <- DBI::dbConnect(
    RSQLite::SQLite(), 
    database
  )
  db_species <- DBI::dbReadTable(con, "species") |>
    dplyr::mutate(
      class = stringr::str_squish(.data$class),
      tax_order = stringr::str_squish(.data$tax_order)
    )
  
  if ("trophic_group" %in% colnames(db_species)) {
    stop(
      "Ecological group has already been added to the database"
    )
  }
  if ("ecological_group" %in% colnames(db_species)) {
    stop(
      "Ecological group has already been added to the database"
    )
  }
  
  # read in trophic groups 
  trophic_groups_file_path <- system.file(
    "extdata/trophic-group.csv",
    package = "wqbench"
  )
  species_trophic_group <- read_trophic_group(
    trophic_groups_file_path, db_species
  )
  
  # create new db things 
  DBI::dbExecute(
    con,
    paste0(
      "CREATE TABLE species_trophic_group ",
      "(", paste(colnames(species_trophic_group), collapse = ", "),
      ", PRIMARY KEY (species_number))"
    )
  )
  
  DBI::dbWriteTable(
    con, 
    "species_trophic_group", 
    value = species_trophic_group, 
    append = TRUE, 
    row.names = FALSE
  )
  if (!quiet) {
    message("Adding: trophic and ecological groups to species table")
  }
  DBI::dbExecute(con, "DROP TABLE species;")
  DBI::dbExecute(
    con,
    "ALTER TABLE species_trophic_group
  RENAME TO species;"
  )
  
  invisible(species_trophic_group)
}

#' Combine Trophic Groups and DB species
#' 
#' Internal to allow for testing
#'
#' @param trophic_groups A data frame
#' @param db_species A data frame
#'
#' @return A data frame
#' 
#' @examples
#' \dontrun{
#' species_trophic_group <- combine_trophic_group(trophic_groups, db_species)
#' }
combine_trophic_group <- function(trophic_groups, db_species) {
  trophic_groups <- trophic_groups |>
    dplyr::mutate(
      class = stringr::str_squish(.data$class),
      order = stringr::str_squish(.data$order),
      trophic_group = stringr::str_squish(.data$trophic_group),
      ecological_group = stringr::str_squish(.data$ecological_group),
    )
  # select group where both have class and order 
  trophic_levels_class_order <- trophic_groups |> 
    dplyr::filter(!is.na(.data$class) & !is.na(.data$order)) |>
    dplyr::rename(
      ecological_group_co = "trophic_group",
      ecological_group_class_co = "ecological_group"
    )
  # select groups with only class 
  trophic_levels_class <- trophic_groups |> 
    dplyr::filter(is.na(.data$order)) |>
    dplyr::rename(
      ecological_group_c = "trophic_group",
      ecological_group_class_c = "ecological_group"
    ) |>
    dplyr::select(
      "class", "ecological_group_c", "ecological_group_class_c"
    )
  # select groups with only order 
  trophic_levels_order <- trophic_groups |> 
    dplyr::filter(is.na(.data$class)) |>
    dplyr::rename(
      ecological_group_o = "trophic_group",
      ecological_group_class_o = "ecological_group"
    ) |>
    dplyr::select(
      "order", "ecological_group_o", "ecological_group_class_o"
    )
  
  # process info
  species_trophic_group <- db_species |>
    dplyr::left_join(trophic_levels_class, by = "class") |>
    dplyr::left_join(
      trophic_levels_class_order, 
      by = c("class", "tax_order" = "order")
    ) |>
    dplyr::left_join(trophic_levels_order, by = c("tax_order" = "order")) |>
    dplyr::mutate(
      ecological_group = dplyr::case_when(
        is.na(.data$ecological_group_class_c) & 
          is.na(.data$ecological_group_class_co) ~ .data$ecological_group_class_o,
        !is.na(.data$ecological_group_class_c) & 
          is.na(ecological_group_class_co) ~ .data$ecological_group_class_c,
        !is.na(.data$ecological_group_class_c) & 
          !is.na(ecological_group_class_co) ~ .data$ecological_group_class_co
      ),
      trophic_group = dplyr::case_when(
        is.na(.data$ecological_group_class_c) & 
          is.na(.data$ecological_group_class_co) ~ .data$ecological_group_o,
        !is.na(.data$ecological_group_class_c) & 
          is.na(.data$ecological_group_class_co) ~ .data$ecological_group_c,
        !is.na(.data$ecological_group_class_c) & 
          !is.na(.data$ecological_group_class_co) ~ .data$ecological_group_co
      )
    ) |>
    dplyr::select(
      dplyr::all_of(colnames(db_species)),
      "ecological_group", "trophic_group",
    ) |>
    tibble::tibble()
}

#' Read Trophic Groups and DB species
#' 
#' Internal to allow for testing
#'
#' @param trophic_groups_file_path A file path
#' @param db_species A data frame
#'
#' @return A data frame
#' 
#' @examples
#' \dontrun{
#' species_trophic_group <- read_trophic_group(trophic_groups_file_path, db_species)
#' }
read_trophic_group <- function(trophic_groups_file_path, db_species) {
  
  trophic_groups <- readr::read_csv(
    trophic_groups_file_path, 
    show_col_types = FALSE
  )
  chk::check_data(
    trophic_groups,
    list(
      class = c("", NA),
      order = c("", NA),
      trophic_group = "",
      ecological_group = ""
    )
  )
  species_trophic_group <- combine_trophic_group(trophic_groups, db_species)
  species_trophic_group
}
