#' Add Trophic Groups to Database
#' 
#' Read in the trophic groups and add them to the SQLite database. Species are 
#' classed into groups and class level groups. 
#'
#' @param database A string to the location of the database.
#' @return Invisible data frame
#' @export
#' @details The trophic group data is contained in a csv file in the extdata 
#' folder of the package. This file can be edited by adding or removing groups 
#' and classes. Do not add new columns, rename columns or rename the file.
#' 
#' The trophic groups file must contain the columns: `class`, `order`, 
#' `ecological_group`, and `ecological_group_class`. The `class` and `order`
#' columns are matched to the `class` and `tax_order` columns in the `species`
#' table of the ECOTOX downloaded data. 
#' 
#' The output table contains three columns: species_number, ecological_group, 
#' and ecological_group_class.
#'
#' The output table is added to the database with the name 
#' `species_trophic_group`.
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
wqb_add_trophic_group <- function(database) {
  chk::chk_file(database)
  chk::chk_ext(database, "sqlite")
  
  # read in trophic groups 
  trophic_groups_file_path <- system.file(
    "extdata/trophic-group.csv",
    package = "wqbench"
  )
  trophic_groups <- readr::read_csv(
    trophic_groups_file_path, 
    show_col_types = FALSE
  )
  chk::check_data(
    trophic_groups,
    list(
      class = c("", NA),
      order = c("", NA),
      ecological_group = "",
      ecological_group_class = ""
    )
  )
  
  # select group where both have class and order 
  trophic_levels_class_order <- trophic_groups |> 
    dplyr::filter(!is.na(class) & !is.na(order)) |>
    dplyr::rename(
      ecological_group_co = "ecological_group",
      ecological_group_class_co = "ecological_group_class"
    )
  # select groups with only class 
  trophic_levels_class <- trophic_groups |> 
    dplyr::filter(is.na(order)) |>
    dplyr::rename(
      ecological_group_c = "ecological_group",
      ecological_group_class_c = "ecological_group_class"
    )
  # select groups with only order 
  trophic_levels_order <- trophic_groups |> 
    dplyr::filter(is.na(class)) |>
    dplyr::rename(
      ecological_group_o = "ecological_group",
      ecological_group_class_o = "ecological_group_class"
    )

  # read in species from db
  on.exit(DBI::dbDisconnect(con))
  con  <- DBI::dbConnect(
    RSQLite::SQLite(), 
    database
  )
  db_species <- DBI::dbReadTable(con, "species")
  
  species_trophic_group <- db_species |>
    dplyr::left_join(trophic_levels_class, by = "class") |>
    dplyr::left_join(
      trophic_levels_class_order, 
      by = c("class", "tax_order" = "order")
    ) |>
    dplyr::left_join(trophic_levels_order, by = c("tax_order" = "order")) |>
    dplyr::mutate(
      ecological_group_class = dplyr::case_when(
        is.na(.data$ecological_group_class_c) & 
          is.na(.data$ecological_group_class_co) ~ .data$ecological_group_class_o,
        !is.na(.data$ecological_group_class_c) & 
          is.na(ecological_group_class_co) ~ .data$ecological_group_class_c,
        !is.na(.data$ecological_group_class_c) & 
          !is.na(ecological_group_class_co) ~ .data$ecological_group_class_co
      ),
      ecological_group = dplyr::case_when(
        is.na(.data$ecological_group_class_c) & 
          is.na(.data$ecological_group_class_co) ~ .data$ecological_group_o,
        !is.na(.data$ecological_group_class_c) & 
          is.na(.data$ecological_group_class_co) ~ .data$ecological_group_c,
        !is.na(.data$ecological_group_class_c) & 
          !is.na(.data$ecological_group_class_co) ~ .data$ecological_group_co
      )
    ) |>
    dplyr::select(
      "species_number", "ecological_group_class", "ecological_group"
    ) |>
    tidyr::drop_na("ecological_group_class", "ecological_group") |>
    tibble::tibble()
  
  DBI::dbExecute(
    con,
    paste0("CREATE TABLE species_trophic_group ",
           "(species_number INTEGER, ecological_group_class TEXT, ",
           "ecological_group TEXT, PRIMARY KEY (species_number))"
    )
  )
  
  DBI::dbWriteTable(
    con, 
    "species_trophic_group", 
    value = species_trophic_group, 
    append = TRUE, 
    row.names = FALSE
  )
  
  invisible(species_trophic_group)
}
