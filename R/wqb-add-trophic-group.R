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
#'   database = "ecotox_ascii_09_15_2022.sqlite"
#' )
#'
#' trophic_group <- wqb_add_trophic_group(
#'   database = "ecotox_db/ecotox_ascii_09_15_2022.sqlite"
#' )
#' }
wqb_add_trophic_group <- function(database, quiet = FALSE) {
  chk::chk_file(database)
  chk::chk_ext(database, "sqlite")

  # read in species from db
  on.exit(DBI::dbDisconnect(con))
  con <- DBI::dbConnect(
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
      # remove potential extra spaces
      phylum_division = stringr::str_squish(.data$phylum_division),
      class = stringr::str_squish(.data$class),
      order = stringr::str_squish(.data$order),
      family = stringr::str_squish(.data$family),
      trophic_group = stringr::str_squish(.data$trophic_group),
      ecological_group = stringr::str_squish(.data$ecological_group),
      # ensure values are missing
      phylum_division = dplyr::if_else(
        .data$phylum_division == "", 
        NA_character_, 
        .data$phylum_division
      ),
      class = dplyr::if_else(
        .data$class == "", 
        NA_character_, 
        .data$class
      ),
      order = dplyr::if_else(
        .data$order == "", 
        NA_character_, 
        .data$order
      ),
      family = dplyr::if_else(
        .data$family == "", 
        NA_character_, 
        .data$family
      ),
      trophic_group = dplyr::if_else(
        .data$trophic_group == "", 
        NA_character_, 
        .data$trophic_group
      ),
      ecological_group = dplyr::if_else(
        .data$ecological_group == "", 
        NA_character_, 
        .data$ecological_group
      )
    )
  
  # create the joins for each category
  df_f <- dplyr::left_join(
    db_species,
    dplyr::filter(
      trophic_groups,
      !is.na(.data$family)
    ),
    by = c("family", "tax_order" =  "order", "class", "phylum_division")
  )

  df_o <- dplyr::left_join(
    db_species,
    dplyr::select(
      dplyr::filter(
        trophic_groups,
        !is.na(.data$order) & is.na(.data$family)
      ),
    -"family"
    ),
    by = c("tax_order" =  "order", "class", "phylum_division")
  )

  df_c <- dplyr::left_join(
    db_species,
    dplyr::select(
      dplyr::filter(
        trophic_groups,
        !is.na(.data$class) & is.na(.data$family) & is.na(.data$order)
      ),
      -"order", -"family"
    ),
    by = c("class", "phylum_division")
  )

  df_p <- dplyr::left_join(
    db_species,
    dplyr::select(
      dplyr::filter(
        trophic_groups,
        !is.na(.data$phylum_division) & is.na(.data$family) & is.na(.data$order) & is.na(.data$class)
      ),
      -"family", -"order", -"class"
    ),
    by = c("phylum_division")
  )

  f_sp_num <-
    df_f |>
    dplyr::filter(!is.na(.data$trophic_group)) |>
    dplyr::select("species_number") |>
    dplyr::distinct() |>
    dplyr::pull()

  o_sp_num <-
    df_o |>
    dplyr::filter(!is.na(.data$trophic_group)) |>
    dplyr::select("species_number") |>
    dplyr::distinct() |>
    dplyr::pull()

  c_sp_num <-
    df_c |>
    dplyr::filter(!is.na(.data$trophic_group)) |>
    dplyr::select("species_number") |>
    dplyr::distinct() |>
    dplyr::pull()

  species_trophic_group <- df_p |>
    # add in class categories
    dplyr::filter(!.data$species_number %in% c_sp_num) |>
    dplyr::bind_rows(
      df_c |> dplyr::filter(!is.na(.data$trophic_group))
    ) |>
    # add in order categories
    dplyr::filter(!.data$species_number %in% o_sp_num) |>
    dplyr::bind_rows(
      df_o |> dplyr::filter(!is.na(.data$trophic_group))
    ) |>
    # add in family categories
    dplyr::filter(!.data$species_number %in% f_sp_num) |>
    dplyr::bind_rows(
      df_f |> dplyr::filter(!is.na(.data$trophic_group))
    )  |>
    dplyr::select(
      dplyr::all_of(colnames(db_species)),
      "ecological_group", "trophic_group",
    ) |>
    dplyr::arrange(.data$species_number) |>
    tibble::tibble()

  species_trophic_group
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
      phylum_division = c("", NA),
      class = c("", NA),
      order = c("", NA),
      family = c("", NA),
      trophic_group = "",
      ecological_group = ""
    )
  )
  species_trophic_group <- combine_trophic_group(trophic_groups, db_species)
  species_trophic_group
}
