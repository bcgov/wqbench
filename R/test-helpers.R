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

skip_if_testing_quick <- function() {
  if (is.null(getOption("wqb_testing"))) {
    invisible(return())
  }

  if (getOption("wqb_testing") == "quick") {
    testthat::skip("Not run: testing mode set to quick")
  }
}

# To skip slower tests while developing set option to quick
options(wqb_testing = "quick")
# Comment out/restart R

create_clean_test_data <- function(update_vals = list(0)) {
  x <- list(
    chemical_name = "Bleach",
    test_cas = NA,
    test_id = NA,
    result_id = NA,
    endpoint = "LOEC",
    effect = NA_character_,
    effect_description = "Mortality",
    conc1_mean = 1,
    conc1_unit = "mg/L",
    conc_conversion_flag = TRUE,
    conc_conversion_value_multiplier = 1,
    conc_conversion_unit = "mg/L",
    conc2_mean = NA,
    conc2_unit = NA,
    conc3_mean = NA,
    conc3_unit = NA,
    duration_mean = 1,
    duration_unit = "h",
    duration_units_to_keep = TRUE,
    duration_value_multiplier_to_hours = 1,
    study_duration_mean = NA,
    study_duration_unit = NA,
    obs_duration_mean = NA,
    obs_duration_unit = NA,
    organism_habitat = NA_character_,
    species_number = 1,
    latin_name = "Oncorhynchus mykiss",
    common_name = "Rainbow Trout",
    kingdom = NA_character_,
    phylum_division = NA_character_,
    subphylum_div = NA_character_,
    superclass = NA_character_,
    class = NA_character_,
    tax_order = NA_character_,
    family = NA_character_,
    genus = "Oncorhynchus",
    species = NA_character_,
    subspecies = NA_character_,
    variety = NA_character_,
    species_present_in_bc = TRUE,
    ecological_group = "Salmonid",
    trophic_group = "Fish",
    lifestage_description = "Adult",
    simple_lifestage = "adult",
    media_type = "FW",
    media_description = "Fresh water",
    media_type_group = "fresh water",
    present_in_bc_wqg = TRUE,
    reference_number = NA,
    reference_type = NA,
    author = NA_character_,
    title = NA_character_,
    source = NA,
    publication_year = NA,
    additional_comments_tests = NA_character_,
    additional_comments_results = NA_character_,
    download_date = NA,
    version = NA_character_
  )
  z <- utils::modifyList(x, update_vals)
  data.frame(z)
}
