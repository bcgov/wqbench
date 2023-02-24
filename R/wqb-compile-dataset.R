#' Compile the data set from the Database 
#'
#' Compile data from the database to make the completed data set from the 
#' external sources and US EPA ECOTOX database.
#'
#' @param database A string to the location of the database.
#' @return Invisible data frame
#' @export
#' @details Combine XXX tables.
#' 
#' Select a subset of columns.
#' 
#' Removes rows with no genus, no concentration, no duration.
#' 
#' Cleans data by removing asterisks in concentration values and endpoints. 
#'
#' @examples
#' \dontrun{
#' data_compiled <- wqb_compile_dataset(
#'  database = "ecotox_ascii_09_15_2022.sqlite"
#' ) 
#' 
#' data_compiled <- wqb_compile_dataset(
#'  database = "ecotox_db/ecotox_ascii_09_15_2022.sqlite"
#' ) 
#' }
wqb_compile_dataset <- function(database) {
  chk::chk_file(database)
  chk::chk_ext(database, "sqlite")
  
  on.exit(DBI::dbDisconnect(con))
  con  <- DBI::dbConnect(
    RSQLite::SQLite(),
    database
  )
  
  db_results <- DBI::dbReadTable(con, "results")
  db_tests <- DBI::dbReadTable(con, "tests")
  
  db_endpoint_codes <- DBI::dbReadTable(con, "endpoint_codes") |>
    dplyr::mutate(
      concentration_flag = as.logical(as.numeric(.data$concentration_flag))
    ) |>
    tibble::tibble()
  db_species <- DBI::dbReadTable(con, "species") |>
    dplyr::mutate(
      species_present_in_bc = as.logical(
        as.numeric(.data$species_present_in_bc)
      )
    ) |>
    tibble::tibble()
  db_lifestage_codes <- DBI::dbReadTable(con, "lifestage_codes") |>
    dplyr::rename("lifestage_description" = "description") |>
    tibble::tibble()
  db_chemicals <- DBI::dbReadTable(con, "chemicals") |>
    dplyr::mutate(
      present_in_bc_wqg = as.logical(as.numeric(.data$present_in_bc_wqg))
    ) |>
    tibble::tibble()
  
  db_references <- DBI::dbReadTable(con, "references")
  db_effect_codes <- DBI::dbReadTable(con, "effect_codes") |>
    dplyr::rename("effect_description" = "description") |>
    tibble::tibble()
  db_media_type_codes <- DBI::dbReadTable(con, "media_type_codes") |>
    dplyr::rename("media_description" = "description") |>
    tibble::tibble()
  
  combined_data <- db_results |>
    # filter to only water  (aquatic) tests
    dplyr::left_join(db_tests, by = "test_id") |>
    dplyr::filter(.data$organism_habitat == "Water") |>
    # filter to on conc endpoints
    dplyr::left_join(db_endpoint_codes, by = c("endpoint" = "code")) |>
    dplyr::filter(.data$concentration_flag) |>
    # clean up asterick endpoints
    dplyr::mutate(endpoint = stringr::str_replace(.data$endpoint, "\\*", "")) |>
    # add species info
    dplyr::left_join(db_species, by = "species_number") |>
    # add life stage info
    dplyr::left_join(
      db_lifestage_codes, by = c("organism_lifestage" = "code")
    ) |>
    # add chemical info
    dplyr::left_join(db_chemicals, by = c("test_cas" = "cas_number")) |>
    # add reference info
    dplyr::left_join(db_references, by = c("reference_number")) |>
    # add effect info
    dplyr::left_join(db_effect_codes, by = c("effect" = "code")) |>
    # add media groups
    dplyr::left_join(db_media_type_codes, by = c("media_type" = "code")) |>
    tibble::tibble()
  
  chk::chk_not_missing(combined_data$organism_lifestage)
  
  # select columns
  selected_columns_data <- combined_data |>
    dplyr::select(
      "chemical_name", "test_cas",
      "test_id", "result_id", "endpoint", "effect", "effect_description",
      "conc1_mean", "conc1_unit",
      "obs_duration_mean", "obs_duration_unit", "study_duration_unit", 
      "organism_habitat",
      "species_number", "latin_name", "common_name", "kingdom", 
      "phylum_division", "subphylum_div", "superclass", "class", "tax_order", 
      "family", "genus", "species", "subspecies", "variety",
      "species_present_in_bc", 
      "ecological_group_class", "ecological_group",
      "lifestage_description", "simple_lifestage", 
      "media_type", "media_description", "media_type_group",
      "present_in_bc_wqg", 
      "reference_number", "reference_type", "author", "title", "source", 
      "publication_year"
    )
  
  compiled_data <- selected_columns_data |>
    # remove missing concentrations
    dplyr::filter(!(.data$conc1_mean == "NR")) |>
    # remove concentration with < or > in them
    dplyr::filter(!(stringr::str_detect(.data$conc1_mean, "\\<|\\>"))) |>
    # remove rows with no species genus
    dplyr::filter(!(.data$genus == "")) |>
    # remove rows with no duration value
    dplyr::filter(!(.data$obs_duration_mean == "")) |> ### double check still valid 
    dplyr::filter(!(.data$obs_duration_mean == "NR")) |> ### double check still valid 
    dplyr::mutate(
      # remove asterisk from end point
      endpoint = stringr::str_replace(.data$endpoint, "\\*", ""),
      # remove asterisk from conc1_mean values and convert to numeric
      conc1_mean = stringr::str_replace(.data$conc1_mean, "\\*", ""),
      conc1_mean = as.numeric(.data$conc1_mean),
      # missing (NA) lifestages should be coded as adult
      # doesn't appear to be any missing but adding in just in case
      simple_lifestage = dplyr::if_else(
        is.na(.data$lifestage_description), 
        "adult", 
        .data$simple_lifestage
      ),
      # simple life stage should only match for amphibians and fish
      simple_lifestage = dplyr::case_when(
        .data$ecological_group == "Invertebrate" ~ NA_character_,
        .data$ecological_group == "Algae" ~ NA_character_,
        .data$ecological_group == "Plant" ~ NA_character_,
        TRUE ~ .data$simple_lifestage
      )
    ) 
  
  compiled_data
}
