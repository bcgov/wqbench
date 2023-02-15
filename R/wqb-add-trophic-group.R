#' Add Trophic Group to Species in the Database
#' 
#' Read in and add the trophic groups to the species. Species are classed into
#' groups and class level groups. 
#'
#' @param file_path A string to the file that contains the trophic information.  
#' @param database A string to the location of the database.
#' @return Invisible data frame
#' @export
#' @details The trophic groups file must contain the columns: class, order, 
#' ecological_group, ecological_group_class.
#' 
#' The output table contains three columns: species_number, ecological_group, 
#' and ecological_group_class.
#'
#' The output table is added to the database with the name 
#' `species_trophic_group`.
#' @examples
#' \dontrun{
#' # all files in root directory
#' trophic_group <- wqb_add_trophic_group(
#'  file_path = "trophic-group.csv",
#'  database = "ecotox_ascii_09_15_2022.sqlite"
#' ) 
#' 
#' # files in subdirectories 
#' trophic_group <- wqb_add_trophic_group(
#'  file_path = "lookups/trophic-group.csv",
#'  database = "ecotox_db/ecotox_ascii_09_15_2022.sqlite"
#' ) 
#' }
wqb_add_trophic_group <- function(file_path, database) {
  chk::chk_file(file_path)
  chk::chk_ext(file_path, "csv")
  chk::chk_file(database)
  chk::chk_ext(database, "sqlite")
  
  # read in trophic groups 
  trophic_groups <- readr::read_csv(file_path, show_col_types = FALSE)
  chk::check_data(
    trophic_groups, 
    class = c("", NA), 
    order = c("", NA), 
    ecological_group = "", 
    ecological_group_class = ""
  )
  
  # select group where both have class and order 
  trophic_levels_class_order <- trophic_groups |> 
    dplyr::filter(!is.na(class) & !is.na(order)) |>
    dplyr::rename(
      ecological_group_co = ecological_group,
      ecological_group_class_co = ecological_group_class
    )
  # select groups with only class 
  trophic_levels_class <- trophic_groups |> 
    dplyr::filter(is.na(order)) |>
    dplyr::rename(
      ecological_group_c = ecological_group,
      ecological_group_class_c = ecological_group_class
    )
  # select groups with only order 
  trophic_levels_order <- trophic_groups |> 
    dplyr::filter(is.na(class)) |>
    dplyr::rename(
      ecological_group_o = ecological_group,
      ecological_group_class_o = ecological_group_class
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
    dplyr::left_join(trophic_levels_class_order, by = c("class", "tax_order" = "order")) |>
    dplyr::left_join(trophic_levels_order, by = c("tax_order" = "order")) |>
    dplyr::mutate(
      ecological_group_class = dplyr::case_when(
        is.na(ecological_group_class_c) & is.na(ecological_group_class_co) ~ ecological_group_class_o,
        !is.na(ecological_group_class_c) & is.na(ecological_group_class_co) ~ ecological_group_class_c,
        !is.na(ecological_group_class_c) & !is.na(ecological_group_class_co) ~ ecological_group_class_co
      ),
      ecological_group = dplyr::case_when(
        is.na(ecological_group_class_c) & is.na(ecological_group_class_co) ~ ecological_group_o,
        !is.na(ecological_group_class_c) & is.na(ecological_group_class_co) ~ ecological_group_c,
        !is.na(ecological_group_class_c) & !is.na(ecological_group_class_co) ~ ecological_group_co
      )
    ) |>
    dplyr::select(
      species_number, ecological_group_class, ecological_group
    ) |>
    tidyr::drop_na(ecological_group_class, ecological_group) |>
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
