#' Download EPA ECOTOX Database Files
#'
#' Download the EPA ECOTOX database files from their FTP site. The default is to
#' download the most recent version of the database.
#'
#' @param file_path A string of the file path location to save the downloaded
#'   files. The default is your current working directory. 
#' @param version A string with the date for the version you want to download.
#'   The default is NULL which downloads the most recent version.
#'
#' @return A string of the file path
#' @export
#'
#' @examples
wqb_download_epa_ecotox <- function(file_path = ".", version = NULL) {
  chk::chk_string(file_path)
  chk::chk_null_or(version, vld = chk::vld_string)
  file_path
}