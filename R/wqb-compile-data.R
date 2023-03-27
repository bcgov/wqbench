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

#' Compile Data 
#'
#' Join database tables together and start filtering and cleaning data.
#'
#' @param database A string to the location of the database.
#' @param quiet Turn off message when quiet set to TRUE.
#' @return Invisible data frame
#' @export
#' @details Check the resource document more details on the data added, 
#' filter conditions and cleaning steps. This is Step 1. 
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
wqb_compile_dataset <- function(database, quiet = FALSE) {

  data <- wqb_join_data(database, quiet)
  data <- wqb_clean_data(data, quiet)
  
  data
}
