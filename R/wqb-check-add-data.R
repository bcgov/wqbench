#' Check the Uploaded Data
#'
#' Checks the uploaded data for the basic requirements to ensure the data
#' matches the downloaded Ecotox data.
#'
#' @param data A data frame. The data you want to check.
#' @param template A data frame. The format the data should be in, in the
#'   [chktemplate] format.
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
  
  check_endpoint(data)
  check_trophic_eco_group(data)
  
 data
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
    dplyr::mutate(code = stringr::str_replace(code, "\\*", "")) |>
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
      paste(unique(trophic_eco_groups$trophic_group), collapse = ", ")
    )
  }
  
  if (!all(data$ecological_group %in% trophic_eco_groups$ecological_group)) {
    chk::abort_chk(
      "The ecological_group column has invalid value(s). The allowed values include: ", 
      paste(unique(trophic_eco_groups$ecological_group), collapse = ", ")
    )
  }
  
  if (!chk::vld_join(data, trophic_eco_groups, by = c("trophic_group", "ecological_group"))) {
    allowed_vals <- trophic_eco_groups  |>
      dplyr::mutate(
        trophic_eco_group = paste(trophic_group, ecological_group, sep = " & ")
      ) |>
      dplyr::pull(trophic_eco_group)
    chk::abort_chk(
      "There is an invalid combination of the trophic_group or ecological_group columns. ",
      "The allowed values include: ", 
       paste(allowed_vals, collapse = ", ")
    )
  }
} 
