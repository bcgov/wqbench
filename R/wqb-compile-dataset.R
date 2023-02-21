#' Compile the data set from the Database 
#'
#' Compile data from the database to make the completed data set from the 
#' external sources and US EPA ECOTOX database.
#'
#' @param database A string to the location of the database.
#' @return Invisible data frame
#' @export
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
  # chk::chk_file(database)
  # chk::chk_ext(database, "sqlite")
  # 
  # on.exit(DBI::dbDisconnect(con))
  # con  <- DBI::dbConnect(
  #   RSQLite::SQLite(),
  #   database
  # )
  # 
  # db_species_british_columbia <- DBI::dbReadTable(con, "species_british_columbia")
  # db_species_trophic_group <- DBI::dbReadTable(con, "species_trophic_group")
  # db_species_tests_media  <- DBI::dbReadTable(con, "species_tests_media")
  # db_tests_aquatic <- DBI::dbReadTable(con, "tests_aquatic")
  # db_endpoint_concentration <- DBI::dbReadTable(con, "endpoint_concentration")
  # #db_results_endpoint_concentration <- DBI::dbReadTable(con, "results_endpoint_concentration")
  # db_lifestage_groups <- DBI::dbReadTable(con, "lifestage_groups")
  # 
  # db_results <- DBI::dbReadTable(con, "results")
  # db_tests <- DBI::dbReadTable(con, "tests")
  # db_references <- DBI::dbReadTable(con, "references")
  # db_species <- DBI::dbReadTable(con, "species")
  # db_chemicals <- DBI::dbReadTable(con, "chemicals")
  # db_effect_codes <- DBI::dbReadTable(con, "effect_codes")
  # db_lifestage_codes <- DBI::dbReadTable(con, "lifestage_codes")
  # 
  # 
  # x <- db_results |>
  #   dplyr::left_join(db_tests, by = "test_id") |>
  #   dplyr::filter(organism_habitat == "Water") |> # filter to only water tests
  #   dplyr::filter(endpoint %in% db_endpoint_concentration$code) |>
  #   dplyr::select(
  #     test_id, test_cas, result_id, endpoint,
  #     effect, conc1_mean, conc1_unit,
  #     obs_duration_mean, obs_duration_unit, study_duration_unit, organism_habitat,
  #     species_number #, latin_name, common_name, kingdom, phylum_division, subphylum_div,
  #     # description_effect,
  #     #superclass, class, tax_order, family, genus, species, subspecies, variety,
  #     #reference_number, reference_type, author, title, source, publication_year
  #   )
  # 
  # sort(unique(x$endpoint))
  # sort(unique(x$organism_habitat))
  # 
  # 
  # 
  
}