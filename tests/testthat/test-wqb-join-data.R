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

test_that("water based tests remain", {
  db_results <- data.frame(
    stringsAsFactors = FALSE,
    test_id = c(1L, 1L, 2L, 2L, 2L),
    result_id = c(4L, 5L, 7L, 8L, 9L),
    endpoint = c("LC50", "LC50", "LC50", "LC50", "LC50"),
    effect = c("MOR", "MOR", "MOR", "MOR", "MOR"),
    conc1_mean = c("10", "0.5", "1", "1.5", "2.0"),
    conc1_unit = c("mg/L", "mg/L", "mg/L", "mg/L", "mg/L"),
    conc2_mean = c(NA, NA, NA, NA, NA),
    conc2_unit = c(NA, NA, NA, NA, NA),
    conc3_mean = c(NA, NA, NA, NA, NA),
    conc3_unit = c(NA, NA, NA, NA, NA),
    obs_duration_mean = c("1", "1", "1", "1", "1"),
    obs_duration_unit = c("d", "d", "d", "d", "d"),
    additional_comments_results = c(" ", " ", " ", " ", " ")
  )

  db_tests <- data.frame(
    stringsAsFactors = FALSE,
    test_cas = c(987, 123, 123),
    test_id = c(1L, 2L, 3L),
    study_duration_mean = c("4", "4", "4"),
    study_duration_unit = c("d", "d", "d"),
    organism_habitat = c("Water", "Non-soil", "Soil"),
    species_number = c(1L, 1L, 1L),
    media_type = c("FW", "FW", "FW"),
    organism_lifestage = c("AD", "AD", "AD"),
    reference_number = c(2L, 2L, 2L),
    additional_comments_tests = c(" ", " ", " ")
  )

  db_endpoint_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("MATC", "LC50", "LC50*", "--", "AC50"),
    description = c(
      "", "Lethal concentration to 50% of test organisms",
      "Lethal concentration to 50% of test organisms",
      "Unspecified",
      "The concentration corresponding to 50% maximal activity. Used in in vitro testing."
    ),
    concentration_flag = c(TRUE, TRUE, TRUE, FALSE, FALSE)
  )

  db_species <- data.frame(
    stringsAsFactors = FALSE,
    species_number = c(1L, 2L),
    latin_name = c("Pimephales promelas", "Lepomis macrochirus"),
    common_name = c("Fathead Minnow", "Bluegill"),
    kingdom = c("Animalia", "Animalia"),
    phylum_division = c("Chordata", "Chordata"),
    subphylum_div = c("Vertebrata", "Vertebrata"),
    superclass = c("Osteichthyes", "Osteichthyes"),
    class = c("Actinopterygii", "Actinopterygii"),
    tax_order = c("Cypriniformes", "Perciformes"),
    family = c("Cyprinidae", "Centrarchidae"),
    genus = c("Pimephales", "Lepomis"),
    species = c("promelas", "macrochirus"),
    subspecies = c(NA, NA),
    variety = c(NA, NA),
    ecotox_group = c(NA, "Fish"),
    species_present_in_bc = c(TRUE, TRUE),
    ecological_group = c("Other", "Salmonid"),
    trophic_group = c("Invertebrate", "Fish")
  )

  db_lifestage_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "AD", "AL", "BD", "BL", "BS"),
    lifestage_description = c(
      "Unspecified", "Adult", "Alevin", "Bud or Budding",
      "Blastula", "Bud blast stage"
    ),
    simple_lifestage = c("adult", "adult", "els", NA, "els", NA)
  )

  db_chemicals <- data.frame(
    stringsAsFactors = FALSE,
    cas_number = c(123, 987, 111),
    chemical_name = c("Ca", "ba", "Za"),
    present_in_bc_wqg = c(FALSE, TRUE, FALSE)
  )

  db_duration_unit_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("eslk", "lhv15-20", "d", "h", "wph"),
    duration_unit_description = c(
      "Early silk stage",
      "Leaf harvest, 15-20 cm", "Day(s)",
      "Hour(s)", "Weeks post-hatch"
    ),
    duration_units_to_keep = c(FALSE, FALSE, FALSE, TRUE, TRUE),
    duration_value_multiplier_to_hours = c(NA, NA, 24, 1, 168)
  )

  db_concentration_unit_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c(
      "acts/3 mi", "ml/100 g bdwt",
      "AI ppm H2O", "ug/L", "mg/L"
    ),
    concentration_unit_description = c(
      "acts per 3 minutes",
      "milliliter per 100 grams body weight",
      "active ingredient parts per million water",
      "micrograms per liter", "miligrams per liter"
    ),
    conc_conversion_flag = c(NA, NA, NA, TRUE, TRUE),
    conc_conversion_value_multiplier = c(NA, NA, NA, 0.001, 1),
    conc_conversion_unit = c(NA, NA, NA, "mg/L", "mg/L")
  )

  db_references <- data.frame(
    stringsAsFactors = FALSE,
    reference_number = c(2L, 4L),
    author = c("A. Smith", "B. Apples"),
    title = c("Reference title 1", "Reference title 2"),
    source = c("Journ. App. Chem.", "Journ. Bio"),
    publication_year = c("1967", "1991")
  )

  db_effect_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "ACC", "POP"),
    effect_description = c("Unspecified", "Accumulation", "Population")
  )

  db_media_type_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "FW", "SW"),
    media_description = c("Unspecified", "Fresh water", "Salt water"),
    media_type_group = c("not reported", "fresh water", "salt water")
  )

  db_meta_data_download <- data.frame(
    stringsAsFactors = FALSE,
    download_date = c("2023-03-30 15:20:20"),
    version = c(" ecotox_ascii_09_15_2022")
  )

  data <- wqbench:::join_data(
    db_results = db_results,
    db_tests = db_tests,
    db_endpoint_codes = db_endpoint_codes,
    db_species = db_species,
    db_lifestage_codes = db_lifestage_codes,
    db_chemicals = db_chemicals,
    db_duration_unit_codes = db_duration_unit_codes,
    db_concentration_unit_codes = db_concentration_unit_codes,
    db_references = db_references,
    db_effect_codes = db_effect_codes,
    db_media_type_codes = db_media_type_codes,
    db_meta_data_download = db_meta_data_download
  )

  expect_equal(
    nrow(data),
    2L
  )
})

test_that("endpoints are joined to give back expected set", {
  db_results <- data.frame(
    stringsAsFactors = FALSE,
    test_id = c(1L, 1L, 2L, 2L, 3L),
    result_id = c(4L, 5L, 7L, 8L, 9L),
    endpoint = c("LC50*/", "MATC", "LC50", "LC50", "AC50"),
    effect = c("MOR", "MOR", "POP", "POP", "MOR"),
    conc1_mean = c("10", "0.5", "1", "1.5", "2.0"),
    conc1_unit = c("ug/L", "ug/L", "mg/L", "mg/L", "ug/L"),
    conc2_mean = c(NA, NA, NA, NA, NA),
    conc2_unit = c(NA, NA, NA, NA, NA),
    conc3_mean = c(NA, NA, NA, NA, NA),
    conc3_unit = c(NA, NA, NA, NA, NA),
    obs_duration_mean = c("48", "96", "4", "1", "1"),
    obs_duration_unit = c("h", "h", "d", "d", "d"),
    additional_comments_results = c(" ", " ", " ", " ", " ")
  )

  db_tests <- data.frame(
    stringsAsFactors = FALSE,
    test_cas = c(987, 123),
    test_id = c(1L, 2L),
    study_duration_mean = c("15", "4"),
    study_duration_unit = c("d", "d"),
    organism_habitat = c("Water", "Water"),
    species_number = c(1L, 1L),
    media_type = c("FW", "FW"),
    organism_lifestage = c("--", "AD"),
    reference_number = c(2L, 2L),
    additional_comments_tests = c(" ", " ")
  )

  db_endpoint_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("MATC", "LC50", "LC50*", "--", "AC50"),
    description = c(
      "", "Lethal concentration to 50% of test organisms",
      "Lethal concentration to 50% of test organisms",
      "Unspecified",
      "The concentration corresponding to 50% maximal activity. Used in in vitro testing."
    ),
    concentration_flag = c(TRUE, TRUE, TRUE, FALSE, FALSE)
  )

  db_species <- data.frame(
    stringsAsFactors = FALSE,
    species_number = c(1L, 2L),
    latin_name = c("Pimephales promelas", "Lepomis macrochirus"),
    common_name = c("Fathead Minnow", "Bluegill"),
    kingdom = c("Animalia", "Animalia"),
    phylum_division = c("Chordata", "Chordata"),
    subphylum_div = c("Vertebrata", "Vertebrata"),
    superclass = c("Osteichthyes", "Osteichthyes"),
    class = c("Actinopterygii", "Actinopterygii"),
    tax_order = c("Cypriniformes", "Perciformes"),
    family = c("Cyprinidae", "Centrarchidae"),
    genus = c("Pimephales", "Lepomis"),
    species = c("promelas", "macrochirus"),
    subspecies = c(NA, NA),
    variety = c(NA, NA),
    ecotox_group = c(NA, "Fish"),
    species_present_in_bc = c(TRUE, TRUE),
    ecological_group = c("Other", "Salmonid"),
    trophic_group = c("Invertebrate", "Fish")
  )

  db_lifestage_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "AD", "AL", "BD", "BL", "BS"),
    lifestage_description = c(
      "Unspecified", "Adult", "Alevin", "Bud or Budding",
      "Blastula", "Bud blast stage"
    ),
    simple_lifestage = c("adult", "adult", "els", NA, "els", NA)
  )

  db_chemicals <- data.frame(
    stringsAsFactors = FALSE,
    cas_number = c(123, 987, 111),
    chemical_name = c("Ca", "ba", "Za"),
    present_in_bc_wqg = c(FALSE, TRUE, FALSE)
  )

  db_duration_unit_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("eslk", "lhv15-20", "d", "h", "wph"),
    duration_unit_description = c(
      "Early silk stage",
      "Leaf harvest, 15-20 cm", "Day(s)",
      "Hour(s)", "Weeks post-hatch"
    ),
    duration_units_to_keep = c(FALSE, FALSE, FALSE, TRUE, TRUE),
    duration_value_multiplier_to_hours = c(NA, NA, 24, 1, 168)
  )

  db_concentration_unit_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c(
      "acts/3 mi", "ml/100 g bdwt",
      "AI ppm H2O", "ug/L", "mg/L"
    ),
    concentration_unit_description = c(
      "acts per 3 minutes",
      "milliliter per 100 grams body weight",
      "active ingredient parts per million water",
      "micrograms per liter", "miligrams per liter"
    ),
    conc_conversion_flag = c(NA, NA, NA, TRUE, TRUE),
    conc_conversion_value_multiplier = c(NA, NA, NA, 0.001, 1),
    conc_conversion_unit = c(NA, NA, NA, "mg/L", "mg/L")
  )

  db_references <- data.frame(
    stringsAsFactors = FALSE,
    reference_number = c(2L, 4L),
    author = c("A. Smith", "B. Apples"),
    title = c("Reference title 1", "Reference title 2"),
    source = c("Journ. App. Chem.", "Journ. Bio"),
    publication_year = c("1967", "1991")
  )

  db_effect_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "ACC", "POP"),
    effect_description = c("Unspecified", "Accumulation", "Population")
  )

  db_media_type_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "FW", "SW"),
    media_description = c("Unspecified", "Fresh water", "Salt water"),
    media_type_group = c("not reported", "fresh water", "salt water")
  )

  db_meta_data_download <- data.frame(
    stringsAsFactors = FALSE,
    download_date = c("2023-03-30 15:20:20"),
    version = c(" ecotox_ascii_09_15_2022")
  )

  data <- wqbench:::join_data(
    db_results = db_results,
    db_tests = db_tests,
    db_endpoint_codes = db_endpoint_codes,
    db_species = db_species,
    db_lifestage_codes = db_lifestage_codes,
    db_chemicals = db_chemicals,
    db_duration_unit_codes = db_duration_unit_codes,
    db_concentration_unit_codes = db_concentration_unit_codes,
    db_references = db_references,
    db_effect_codes = db_effect_codes,
    db_media_type_codes = db_media_type_codes,
    db_meta_data_download = db_meta_data_download
  )

  expect_equal(
    nrow(data),
    4L
  )
})

test_that("species are joined by species number", {
  db_results <- data.frame(
    stringsAsFactors = FALSE,
    test_id = c(1L, 1L, 2L, 2L, 3L),
    result_id = c(4L, 5L, 7L, 8L, 9L),
    endpoint = c("LC50", "LC50", "LC50", "LC50", "LC50"),
    effect = c("MOR", "MOR", "MOR", "MOR", "MOR"),
    conc1_mean = c("1", "1", "1", "1", "1"),
    conc1_unit = c("mg/L", "mg/L", "mg/L", "mg/L", "mg/L"),
    conc2_mean = c(NA, NA, NA, NA, NA),
    conc2_unit = c(NA, NA, NA, NA, NA),
    conc3_mean = c(NA, NA, NA, NA, NA),
    conc3_unit = c(NA, NA, NA, NA, NA),
    obs_duration_mean = c("1", "1", "1", "1", "1"),
    obs_duration_unit = c("d", "d", "d", "d", "d"),
    additional_comments_results = c(" ", " ", " ", " ", " ")
  )
  db_tests <- data.frame(
    stringsAsFactors = FALSE,
    test_cas = c(987, 123, 123),
    test_id = c(1L, 2L, 3L),
    study_duration_mean = c("4", "4", "4"),
    study_duration_unit = c("d", "d", "d"),
    organism_habitat = c("Water", "Water", "Water"),
    species_number = c(1L, 2L, 4L),
    media_type = c("FW", "FW", "FW"),
    organism_lifestage = c("AD", "AD", "AD"),
    reference_number = c(2L, 2L, 2L),
    additional_comments_tests = c(" ", " ", " ")
  )
  db_endpoint_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("MATC", "LC50", "LC50*", "--", "AC50"),
    description = c(
      "", "Lethal concentration to 50% of test organisms",
      "Lethal concentration to 50% of test organisms",
      "Unspecified",
      "The concentration corresponding to 50% maximal activity. Used in in vitro testing."
    ),
    concentration_flag = c(TRUE, TRUE, TRUE, FALSE, FALSE)
  )
  db_species <- data.frame(
    stringsAsFactors = FALSE,
    species_number = c(1L, 2L, 3L, 4L),
    latin_name = c(
      "Pimephales promelas", "Lepomis macrochirus", "Hyalella azteca",
      "Orconectes nais"
    ),
    common_name = c("Fathead Minnow", "Bluegill", "Scud", "Crayfish"),
    kingdom = c("Animalia", "Animalia", "Animalia", "Animalia"),
    phylum_division = c("Chordata", "Chordata", "Arthropoda", "Arthropoda"),
    subphylum_div = c("Vertebrata", "Vertebrata", "Crustacea", "Crustacea"),
    superclass = c("Osteichthyes", "Osteichthyes", NA, NA),
    class = c("Actinopterygii", "Actinopterygii", "Malacostraca", "Malacostraca"),
    tax_order = c("Cypriniformes", "Perciformes", "Amphipoda", "Decapoda"),
    family = c("Cyprinidae", "Centrarchidae", "Hyalellidae", "Cambaridae"),
    genus = c("Pimephales", "Lepomis", "Hyalella", "Orconectes"),
    species = c("promelas", "macrochirus", "azteca", "nais"),
    subspecies = c(NA, NA, NA, NA),
    variety = c(NA, NA, NA, NA),
    ecotox_group = c(NA, "Fish", NA, NA),
    species_present_in_bc = c(TRUE, TRUE, FALSE, FALSE),
    ecological_group = c("Other", "Salmonid", "Other", "Planktonic Invertebrate"),
    trophic_group = c("Fish", "Fish", "Invertebrate", "Invertebrate")
  )
  db_lifestage_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "AD", "AL", "BD", "BL", "BS"),
    lifestage_description = c(
      "Unspecified", "Adult", "Alevin", "Bud or Budding",
      "Blastula", "Bud blast stage"
    ),
    simple_lifestage = c("adult", "adult", "els", NA, "els", NA)
  )
  db_chemicals <- data.frame(
    stringsAsFactors = FALSE,
    cas_number = c(123, 987, 111),
    chemical_name = c("Ca", "ba", "Za"),
    present_in_bc_wqg = c(FALSE, TRUE, FALSE)
  )
  db_duration_unit_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("eslk", "lhv15-20", "d", "h", "wph"),
    duration_unit_description = c(
      "Early silk stage",
      "Leaf harvest, 15-20 cm", "Day(s)",
      "Hour(s)", "Weeks post-hatch"
    ),
    duration_units_to_keep = c(FALSE, FALSE, FALSE, TRUE, TRUE),
    duration_value_multiplier_to_hours = c(NA, NA, 24, 1, 168)
  )
  db_concentration_unit_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c(
      "acts/3 mi", "ml/100 g bdwt",
      "AI ppm H2O", "ug/L", "mg/L"
    ),
    concentration_unit_description = c(
      "acts per 3 minutes",
      "milliliter per 100 grams body weight",
      "active ingredient parts per million water",
      "micrograms per liter", "miligrams per liter"
    ),
    conc_conversion_flag = c(NA, NA, NA, TRUE, TRUE),
    conc_conversion_value_multiplier = c(NA, NA, NA, 0.001, 1),
    conc_conversion_unit = c(NA, NA, NA, "mg/L", "mg/L")
  )
  db_references <- data.frame(
    stringsAsFactors = FALSE,
    reference_number = c(2L, 4L),
    author = c("A. Smith", "B. Apples"),
    title = c("Reference title 1", "Reference title 2"),
    source = c("Journ. App. Chem.", "Journ. Bio"),
    publication_year = c("1967", "1991")
  )
  db_effect_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "ACC", "POP"),
    effect_description = c("Unspecified", "Accumulation", "Population")
  )
  db_media_type_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "FW", "SW"),
    media_description = c("Unspecified", "Fresh water", "Salt water"),
    media_type_group = c("not reported", "fresh water", "salt water")
  )
  db_meta_data_download <- data.frame(
    stringsAsFactors = FALSE,
    download_date = c("2023-03-30 15:20:20"),
    version = c(" ecotox_ascii_09_15_2022")
  )

  data <- wqbench:::join_data(
    db_results = db_results,
    db_tests = db_tests,
    db_endpoint_codes = db_endpoint_codes,
    db_species = db_species,
    db_lifestage_codes = db_lifestage_codes,
    db_chemicals = db_chemicals,
    db_duration_unit_codes = db_duration_unit_codes,
    db_concentration_unit_codes = db_concentration_unit_codes,
    db_references = db_references,
    db_effect_codes = db_effect_codes,
    db_media_type_codes = db_media_type_codes,
    db_meta_data_download = db_meta_data_download
  )
  expect_equal(
    data[data$species_number == 1 & data$result_id == 4, ]$latin_name,
    "Pimephales promelas"
  )
  expect_equal(
    data[data$species_number == 2 & data$result_id == 7, ]$latin_name,
    "Lepomis macrochirus"
  )
  expect_equal(
    data[data$species_number == 4 & data$result_id == 9, ]$latin_name,
    "Orconectes nais"
  )
})

test_that("organism lifestage are joined by species number", {
  db_results <- data.frame(
    stringsAsFactors = FALSE,
    test_id = c(1L, 1L, 2L, 3L, 4L),
    result_id = c(4L, 5L, 7L, 8L, 9L),
    endpoint = c("LC50", "LC50", "LC50", "LC50", "LC50"),
    effect = c("MOR", "MOR", "MOR", "MOR", "MOR"),
    conc1_mean = c("1", "1", "1", "1", "1"),
    conc1_unit = c("mg/L", "mg/L", "mg/L", "mg/L", "mg/L"),
    conc2_mean = c(NA, NA, NA, NA, NA),
    conc2_unit = c(NA, NA, NA, NA, NA),
    conc3_mean = c(NA, NA, NA, NA, NA),
    conc3_unit = c(NA, NA, NA, NA, NA),
    obs_duration_mean = c("1", "1", "1", "1", "1"),
    obs_duration_unit = c("d", "d", "d", "d", "d"),
    additional_comments_results = c(" ", " ", " ", " ", " ")
  )
  db_tests <- data.frame(
    stringsAsFactors = FALSE,
    test_cas = c(987, 123, 123, 987),
    test_id = c(1L, 2L, 3L, 4L),
    study_duration_mean = c("4", "4", "4", "4"),
    study_duration_unit = c("d", "d", "d", "d"),
    organism_habitat = c("Water", "Water", "Water", "Water"),
    species_number = c(1L, 2L, 4L, 3L),
    media_type = c("FW", "FW", "FW", "FW"),
    organism_lifestage = c("--", "AD", "BT", "AL"),
    reference_number = c(2L, 2L, 2L, 2L),
    additional_comments_tests = c(" ", " ", " ", " ")
  )
  db_endpoint_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("MATC", "LC50", "LC50*", "--", "AC50"),
    description = c(
      "", "Lethal concentration to 50% of test organisms",
      "Lethal concentration to 50% of test organisms",
      "Unspecified",
      "The concentration corresponding to 50% maximal activity. Used in in vitro testing."
    ),
    concentration_flag = c(TRUE, TRUE, TRUE, FALSE, FALSE)
  )
  db_species <- data.frame(
    stringsAsFactors = FALSE,
    species_number = c(1L, 2L, 3L, 4L),
    latin_name = c(
      "Pimephales promelas", "Lepomis macrochirus", "Hyalella azteca",
      "Orconectes nais"
    ),
    common_name = c("Fathead Minnow", "Bluegill", "Scud", "Crayfish"),
    kingdom = c("Animalia", "Animalia", "Animalia", "Animalia"),
    phylum_division = c("Chordata", "Chordata", "Arthropoda", "Arthropoda"),
    subphylum_div = c("Vertebrata", "Vertebrata", "Crustacea", "Crustacea"),
    superclass = c("Osteichthyes", "Osteichthyes", NA, NA),
    class = c("Actinopterygii", "Actinopterygii", "Malacostraca", "Malacostraca"),
    tax_order = c("Cypriniformes", "Perciformes", "Amphipoda", "Decapoda"),
    family = c("Cyprinidae", "Centrarchidae", "Hyalellidae", "Cambaridae"),
    genus = c("Pimephales", "Lepomis", "Hyalella", "Orconectes"),
    species = c("promelas", "macrochirus", "azteca", "nais"),
    subspecies = c(NA, NA, NA, NA),
    variety = c(NA, NA, NA, NA),
    ecotox_group = c(NA, "Fish", NA, NA),
    species_present_in_bc = c(TRUE, TRUE, FALSE, FALSE),
    ecological_group = c("Other", "Salmonid", "Other", "Planktonic Invertebrate"),
    trophic_group = c("Fish", "Fish", "Invertebrate", "Invertebrate")
  )
  db_lifestage_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "AD", "AL", "BD", "BL", "BS"),
    lifestage_description = c(
      "Unspecified", "Adult", "Alevin", "Bud or Budding",
      "Blastula", "Bud blast stage"
    ),
    simple_lifestage = c("adult", "adult", "els", NA, "els", NA)
  )
  db_chemicals <- data.frame(
    stringsAsFactors = FALSE,
    cas_number = c(123, 987, 111),
    chemical_name = c("Ca", "ba", "Za"),
    present_in_bc_wqg = c(FALSE, TRUE, FALSE)
  )
  db_duration_unit_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("eslk", "lhv15-20", "d", "h", "wph"),
    duration_unit_description = c(
      "Early silk stage",
      "Leaf harvest, 15-20 cm", "Day(s)",
      "Hour(s)", "Weeks post-hatch"
    ),
    duration_units_to_keep = c(FALSE, FALSE, FALSE, TRUE, TRUE),
    duration_value_multiplier_to_hours = c(NA, NA, 24, 1, 168)
  )
  db_concentration_unit_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c(
      "acts/3 mi", "ml/100 g bdwt",
      "AI ppm H2O", "ug/L", "mg/L"
    ),
    concentration_unit_description = c(
      "acts per 3 minutes",
      "milliliter per 100 grams body weight",
      "active ingredient parts per million water",
      "micrograms per liter", "miligrams per liter"
    ),
    conc_conversion_flag = c(NA, NA, NA, TRUE, TRUE),
    conc_conversion_value_multiplier = c(NA, NA, NA, 0.001, 1),
    conc_conversion_unit = c(NA, NA, NA, "mg/L", "mg/L")
  )
  db_references <- data.frame(
    stringsAsFactors = FALSE,
    reference_number = c(2L, 4L),
    author = c("A. Smith", "B. Apples"),
    title = c("Reference title 1", "Reference title 2"),
    source = c("Journ. App. Chem.", "Journ. Bio"),
    publication_year = c("1967", "1991")
  )
  db_effect_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "ACC", "POP"),
    effect_description = c("Unspecified", "Accumulation", "Population")
  )
  db_media_type_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "FW", "SW"),
    media_description = c("Unspecified", "Fresh water", "Salt water"),
    media_type_group = c("not reported", "fresh water", "salt water")
  )
  db_meta_data_download <- data.frame(
    stringsAsFactors = FALSE,
    download_date = c("2023-03-30 15:20:20"),
    version = c(" ecotox_ascii_09_15_2022")
  )

  data <- wqbench:::join_data(
    db_results = db_results,
    db_tests = db_tests,
    db_endpoint_codes = db_endpoint_codes,
    db_species = db_species,
    db_lifestage_codes = db_lifestage_codes,
    db_chemicals = db_chemicals,
    db_duration_unit_codes = db_duration_unit_codes,
    db_concentration_unit_codes = db_concentration_unit_codes,
    db_references = db_references,
    db_effect_codes = db_effect_codes,
    db_media_type_codes = db_media_type_codes,
    db_meta_data_download = db_meta_data_download
  )
  expect_equal(
    data[data$lifestage_description == "Unspecified" & !is.na(data$lifestage_description), ]$simple_lifestage,
    c("adult", "adult")
  )
  expect_equal(
    data[data$lifestage_description == "Adult" & !is.na(data$lifestage_description), ]$simple_lifestage,
    c("adult")
  )
  expect_equal(
    data[data$lifestage_description == "Alevin" & !is.na(data$lifestage_description), ]$simple_lifestage,
    c("els")
  )
})


test_that("chemicals are joined by cas number", {
  db_results <- data.frame(
    stringsAsFactors = FALSE,
    test_id = c(1L, 1L, 2L, 3L, 4L),
    result_id = c(4L, 5L, 7L, 8L, 9L),
    endpoint = c("LC50", "LC50", "LC50", "LC50", "LC50"),
    effect = c("MOR", "MOR", "MOR", "MOR", "MOR"),
    conc1_mean = c("1", "1", "1", "1", "1"),
    conc1_unit = c("mg/L", "mg/L", "mg/L", "mg/L", "mg/L"),
    conc2_mean = c(NA, NA, NA, NA, NA),
    conc2_unit = c(NA, NA, NA, NA, NA),
    conc3_mean = c(NA, NA, NA, NA, NA),
    conc3_unit = c(NA, NA, NA, NA, NA),
    obs_duration_mean = c("1", "1", "1", "1", "1"),
    obs_duration_unit = c("d", "d", "d", "d", "d"),
    additional_comments_results = c(" ", " ", " ", " ", " ")
  )
  db_tests <- data.frame(
    stringsAsFactors = FALSE,
    test_cas = c(987, 123, 123, 111),
    test_id = c(1L, 2L, 3L, 4L),
    study_duration_mean = c("4", "4", "4", "4"),
    study_duration_unit = c("d", "d", "d", "d"),
    organism_habitat = c("Water", "Water", "Water", "Water"),
    species_number = c(1L, 2L, 4L, 3L),
    media_type = c("FW", "FW", "FW", "FW"),
    organism_lifestage = c("--", "AD", "BT", "AL"),
    reference_number = c(2L, 2L, 2L, 2L),
    additional_comments_tests = c(" ", " ", " ", " ")
  )
  db_endpoint_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("MATC", "LC50", "LC50*", "--", "AC50"),
    description = c(
      "", "Lethal concentration to 50% of test organisms",
      "Lethal concentration to 50% of test organisms",
      "Unspecified",
      "The concentration corresponding to 50% maximal activity. Used in in vitro testing."
    ),
    concentration_flag = c(TRUE, TRUE, TRUE, FALSE, FALSE)
  )
  db_species <- data.frame(
    stringsAsFactors = FALSE,
    species_number = c(1L, 2L, 3L, 4L),
    latin_name = c(
      "Pimephales promelas", "Lepomis macrochirus", "Hyalella azteca",
      "Orconectes nais"
    ),
    common_name = c("Fathead Minnow", "Bluegill", "Scud", "Crayfish"),
    kingdom = c("Animalia", "Animalia", "Animalia", "Animalia"),
    phylum_division = c("Chordata", "Chordata", "Arthropoda", "Arthropoda"),
    subphylum_div = c("Vertebrata", "Vertebrata", "Crustacea", "Crustacea"),
    superclass = c("Osteichthyes", "Osteichthyes", NA, NA),
    class = c("Actinopterygii", "Actinopterygii", "Malacostraca", "Malacostraca"),
    tax_order = c("Cypriniformes", "Perciformes", "Amphipoda", "Decapoda"),
    family = c("Cyprinidae", "Centrarchidae", "Hyalellidae", "Cambaridae"),
    genus = c("Pimephales", "Lepomis", "Hyalella", "Orconectes"),
    species = c("promelas", "macrochirus", "azteca", "nais"),
    subspecies = c(NA, NA, NA, NA),
    variety = c(NA, NA, NA, NA),
    ecotox_group = c(NA, "Fish", NA, NA),
    species_present_in_bc = c(TRUE, TRUE, FALSE, FALSE),
    ecological_group = c("Other", "Salmonid", "Other", "Planktonic Invertebrate"),
    trophic_group = c("Fish", "Fish", "Invertebrate", "Invertebrate")
  )
  db_lifestage_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "AD", "AL", "BD", "BL", "BS"),
    lifestage_description = c(
      "Unspecified", "Adult", "Alevin", "Bud or Budding",
      "Blastula", "Bud blast stage"
    ),
    simple_lifestage = c("adult", "adult", "els", NA, "els", NA)
  )
  db_chemicals <- data.frame(
    stringsAsFactors = FALSE,
    cas_number = c(123, 987, 111),
    chemical_name = c("Ca", "ba", "Za"),
    present_in_bc_wqg = c(FALSE, TRUE, FALSE)
  )
  db_duration_unit_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("eslk", "lhv15-20", "d", "h", "wph"),
    duration_unit_description = c(
      "Early silk stage",
      "Leaf harvest, 15-20 cm", "Day(s)",
      "Hour(s)", "Weeks post-hatch"
    ),
    duration_units_to_keep = c(FALSE, FALSE, FALSE, TRUE, TRUE),
    duration_value_multiplier_to_hours = c(NA, NA, 24, 1, 168)
  )
  db_concentration_unit_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c(
      "acts/3 mi", "ml/100 g bdwt",
      "AI ppm H2O", "ug/L", "mg/L"
    ),
    concentration_unit_description = c(
      "acts per 3 minutes",
      "milliliter per 100 grams body weight",
      "active ingredient parts per million water",
      "micrograms per liter", "miligrams per liter"
    ),
    conc_conversion_flag = c(NA, NA, NA, TRUE, TRUE),
    conc_conversion_value_multiplier = c(NA, NA, NA, 0.001, 1),
    conc_conversion_unit = c(NA, NA, NA, "mg/L", "mg/L")
  )
  db_references <- data.frame(
    stringsAsFactors = FALSE,
    reference_number = c(2L, 4L),
    author = c("A. Smith", "B. Apples"),
    title = c("Reference title 1", "Reference title 2"),
    source = c("Journ. App. Chem.", "Journ. Bio"),
    publication_year = c("1967", "1991")
  )
  db_effect_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "ACC", "POP"),
    effect_description = c("Unspecified", "Accumulation", "Population")
  )
  db_media_type_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "FW", "SW"),
    media_description = c("Unspecified", "Fresh water", "Salt water"),
    media_type_group = c("not reported", "fresh water", "salt water")
  )
  db_meta_data_download <- data.frame(
    stringsAsFactors = FALSE,
    download_date = c("2023-03-30 15:20:20"),
    version = c(" ecotox_ascii_09_15_2022")
  )

  data <- wqbench:::join_data(
    db_results = db_results,
    db_tests = db_tests,
    db_endpoint_codes = db_endpoint_codes,
    db_species = db_species,
    db_lifestage_codes = db_lifestage_codes,
    db_chemicals = db_chemicals,
    db_duration_unit_codes = db_duration_unit_codes,
    db_concentration_unit_codes = db_concentration_unit_codes,
    db_references = db_references,
    db_effect_codes = db_effect_codes,
    db_media_type_codes = db_media_type_codes,
    db_meta_data_download = db_meta_data_download
  )
  expect_equal(
    data[data$test_cas == 987, ]$chemical_name,
    c("ba", "ba")
  )
  expect_equal(
    data[data$test_cas == 987 & data$test_id == 1, ]$chemical_name,
    c("ba", "ba")
  )
  expect_equal(
    data[data$test_cas == 123, ]$chemical_name,
    c("Ca", "Ca")
  )
  expect_equal(
    data[data$test_cas == 123 & data$test_id == 2, ]$chemical_name,
    c("Ca")
  )
  expect_equal(
    data[data$test_cas == 111, ]$chemical_name,
    c("Za")
  )
})

test_that("duration is taken from study first and then observed if missing", {
  db_results <- data.frame(
    stringsAsFactors = FALSE,
    test_id = c(1L, 1L, 2L, 3L, 4L, 5L),
    result_id = c(4L, 5L, 7L, 8L, 9L, 10L),
    endpoint = c("LC50", "LC50", "LC50", "LC50", "LC50", "LC50"),
    effect = c("MOR", "MOR", "MOR", "MOR", "MOR", "MOR"),
    conc1_mean = c("1", "1", "1", "1", "1", "1"),
    conc1_unit = c("mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L"),
    conc2_mean = c(NA, NA, NA, NA, NA, NA),
    conc2_unit = c(NA, NA, NA, NA, NA, NA),
    conc3_mean = c(NA, NA, NA, NA, NA, NA),
    conc3_unit = c(NA, NA, NA, NA, NA, NA),
    obs_duration_mean = c("1", "6", NA, "NC", "1", "4"),
    obs_duration_unit = c("d", "h", NA, "NC", "d", "h"),
    additional_comments_results = c(" ", " ", " ", " ", " ", " ")
  )
  db_tests <- data.frame(
    stringsAsFactors = FALSE,
    test_cas = c(987, 123, 123, 111, 111),
    test_id = c(1L, 2L, 3L, 4L, 5L),
    study_duration_mean = c("1", "2", "3", "NC", NA),
    study_duration_unit = c("d", "d", "d", "NC", NA),
    organism_habitat = c("Water", "Water", "Water", "Water", "Water"),
    species_number = c(1L, 2L, 4L, 3L, 2L),
    media_type = c("FW", "FW", "FW", "FW", "FW"),
    organism_lifestage = c("AD", "AD", "AD", "AD", "AD"),
    reference_number = c(2L, 2L, 2L, 2L, 2L),
    additional_comments_tests = c(" ", " ", " ", " ", " ")
  )
  db_endpoint_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("MATC", "LC50", "LC50*", "--", "AC50"),
    description = c(
      "", "Lethal concentration to 50% of test organisms",
      "Lethal concentration to 50% of test organisms",
      "Unspecified",
      "The concentration corresponding to 50% maximal activity. Used in in vitro testing."
    ),
    concentration_flag = c(TRUE, TRUE, TRUE, FALSE, FALSE)
  )
  db_species <- data.frame(
    stringsAsFactors = FALSE,
    species_number = c(1L, 2L, 3L, 4L),
    latin_name = c(
      "Pimephales promelas", "Lepomis macrochirus", "Hyalella azteca",
      "Orconectes nais"
    ),
    common_name = c("Fathead Minnow", "Bluegill", "Scud", "Crayfish"),
    kingdom = c("Animalia", "Animalia", "Animalia", "Animalia"),
    phylum_division = c("Chordata", "Chordata", "Arthropoda", "Arthropoda"),
    subphylum_div = c("Vertebrata", "Vertebrata", "Crustacea", "Crustacea"),
    superclass = c("Osteichthyes", "Osteichthyes", NA, NA),
    class = c("Actinopterygii", "Actinopterygii", "Malacostraca", "Malacostraca"),
    tax_order = c("Cypriniformes", "Perciformes", "Amphipoda", "Decapoda"),
    family = c("Cyprinidae", "Centrarchidae", "Hyalellidae", "Cambaridae"),
    genus = c("Pimephales", "Lepomis", "Hyalella", "Orconectes"),
    species = c("promelas", "macrochirus", "azteca", "nais"),
    subspecies = c(NA, NA, NA, NA),
    variety = c(NA, NA, NA, NA),
    ecotox_group = c(NA, "Fish", NA, NA),
    species_present_in_bc = c(TRUE, TRUE, FALSE, FALSE),
    ecological_group = c("Other", "Salmonid", "Other", "Planktonic Invertebrate"),
    trophic_group = c("Fish", "Fish", "Invertebrate", "Invertebrate")
  )
  db_lifestage_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "AD", "AL", "BD", "BL", "BS"),
    lifestage_description = c(
      "Unspecified", "Adult", "Alevin", "Bud or Budding",
      "Blastula", "Bud blast stage"
    ),
    simple_lifestage = c("adult", "adult", "els", NA, "els", NA)
  )
  db_chemicals <- data.frame(
    stringsAsFactors = FALSE,
    cas_number = c(123, 987, 111),
    chemical_name = c("Ca", "ba", "Za"),
    present_in_bc_wqg = c(FALSE, TRUE, FALSE)
  )
  db_duration_unit_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("eslk", "lhv15-20", "d", "h", "wph"),
    duration_unit_description = c(
      "Early silk stage",
      "Leaf harvest, 15-20 cm", "Day(s)",
      "Hour(s)", "Weeks post-hatch"
    ),
    duration_units_to_keep = c(FALSE, FALSE, FALSE, TRUE, TRUE),
    duration_value_multiplier_to_hours = c(NA, NA, 24, 1, 168)
  )
  db_concentration_unit_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c(
      "acts/3 mi", "ml/100 g bdwt",
      "AI ppm H2O", "ug/L", "mg/L"
    ),
    concentration_unit_description = c(
      "acts per 3 minutes",
      "milliliter per 100 grams body weight",
      "active ingredient parts per million water",
      "micrograms per liter", "miligrams per liter"
    ),
    conc_conversion_flag = c(NA, NA, NA, TRUE, TRUE),
    conc_conversion_value_multiplier = c(NA, NA, NA, 0.001, 1),
    conc_conversion_unit = c(NA, NA, NA, "mg/L", "mg/L")
  )
  db_references <- data.frame(
    stringsAsFactors = FALSE,
    reference_number = c(2L, 4L),
    author = c("A. Smith", "B. Apples"),
    title = c("Reference title 1", "Reference title 2"),
    source = c("Journ. App. Chem.", "Journ. Bio"),
    publication_year = c("1967", "1991")
  )
  db_effect_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "ACC", "POP"),
    effect_description = c("Unspecified", "Accumulation", "Population")
  )
  db_media_type_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "FW", "SW"),
    media_description = c("Unspecified", "Fresh water", "Salt water"),
    media_type_group = c("not reported", "fresh water", "salt water")
  )
  db_meta_data_download <- data.frame(
    stringsAsFactors = FALSE,
    download_date = c("2023-03-30 15:20:20"),
    version = c(" ecotox_ascii_09_15_2022")
  )

  data <- wqbench:::join_data(
    db_results = db_results,
    db_tests = db_tests,
    db_endpoint_codes = db_endpoint_codes,
    db_species = db_species,
    db_lifestage_codes = db_lifestage_codes,
    db_chemicals = db_chemicals,
    db_duration_unit_codes = db_duration_unit_codes,
    db_concentration_unit_codes = db_concentration_unit_codes,
    db_references = db_references,
    db_effect_codes = db_effect_codes,
    db_media_type_codes = db_media_type_codes,
    db_meta_data_download = db_meta_data_download
  )
  expect_equal(
    data$duration_mean,
    c("1", "1", "2", "3", "1", "4")
  )
  expect_equal(
    data$duration_unit,
    c("d", "d", "d", "d", "d", "h")
  )
})

test_that("duration codes are joined properly to duration unit", {
  db_results <- data.frame(
    stringsAsFactors = FALSE,
    test_id = c(1L, 1L, 2L, 3L, 4L, 5L),
    result_id = c(4L, 5L, 7L, 8L, 9L, 10L),
    endpoint = c("LC50", "LC50", "LC50", "LC50", "LC50", "LC50"),
    effect = c("MOR", "MOR", "MOR", "MOR", "MOR", "MOR"),
    conc1_mean = c("1", "1", "1", "1", "1", "1"),
    conc1_unit = c("mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L"),
    conc2_mean = c(NA, NA, NA, NA, NA, NA),
    conc2_unit = c(NA, NA, NA, NA, NA, NA),
    conc3_mean = c(NA, NA, NA, NA, NA, NA),
    conc3_unit = c(NA, NA, NA, NA, NA, NA),
    obs_duration_mean = c("1", "1", NA, "NC", "1", "1"),
    obs_duration_unit = c("lhv15-20", "h", NA, "NC", "d", "h"),
    additional_comments_results = c(" ", " ", " ", " ", " ", " ")
  )
  db_tests <- data.frame(
    stringsAsFactors = FALSE,
    test_cas = c(987, 123, 123, 111, 111),
    test_id = c(1L, 2L, 3L, 4L, 5L),
    study_duration_mean = c("1", "1", "1", "NC", NA),
    study_duration_unit = c("d", "d", "lhv15-20", "NC", NA),
    organism_habitat = c("Water", "Water", "Water", "Water", "Water"),
    species_number = c(1L, 2L, 4L, 3L, 2L),
    media_type = c("FW", "FW", "FW", "FW", "FW"),
    organism_lifestage = c("AD", "AD", "AD", "AD", "AD"),
    reference_number = c(2L, 2L, 2L, 2L, 2L),
    additional_comments_tests = c(" ", " ", " ", " ", " ")
  )
  db_endpoint_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("MATC", "LC50", "LC50*", "--", "AC50"),
    description = c(
      "", "Lethal concentration to 50% of test organisms",
      "Lethal concentration to 50% of test organisms",
      "Unspecified",
      "The concentration corresponding to 50% maximal activity. Used in in vitro testing."
    ),
    concentration_flag = c(TRUE, TRUE, TRUE, FALSE, FALSE)
  )
  db_species <- data.frame(
    stringsAsFactors = FALSE,
    species_number = c(1L, 2L, 3L, 4L),
    latin_name = c(
      "Pimephales promelas", "Lepomis macrochirus", "Hyalella azteca",
      "Orconectes nais"
    ),
    common_name = c("Fathead Minnow", "Bluegill", "Scud", "Crayfish"),
    kingdom = c("Animalia", "Animalia", "Animalia", "Animalia"),
    phylum_division = c("Chordata", "Chordata", "Arthropoda", "Arthropoda"),
    subphylum_div = c("Vertebrata", "Vertebrata", "Crustacea", "Crustacea"),
    superclass = c("Osteichthyes", "Osteichthyes", NA, NA),
    class = c("Actinopterygii", "Actinopterygii", "Malacostraca", "Malacostraca"),
    tax_order = c("Cypriniformes", "Perciformes", "Amphipoda", "Decapoda"),
    family = c("Cyprinidae", "Centrarchidae", "Hyalellidae", "Cambaridae"),
    genus = c("Pimephales", "Lepomis", "Hyalella", "Orconectes"),
    species = c("promelas", "macrochirus", "azteca", "nais"),
    subspecies = c(NA, NA, NA, NA),
    variety = c(NA, NA, NA, NA),
    ecotox_group = c(NA, "Fish", NA, NA),
    species_present_in_bc = c(TRUE, TRUE, FALSE, FALSE),
    ecological_group = c("Other", "Salmonid", "Other", "Planktonic Invertebrate"),
    trophic_group = c("Fish", "Fish", "Invertebrate", "Invertebrate")
  )
  db_lifestage_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "AD", "AL", "BD", "BL", "BS"),
    lifestage_description = c(
      "Unspecified", "Adult", "Alevin", "Bud or Budding",
      "Blastula", "Bud blast stage"
    ),
    simple_lifestage = c("adult", "adult", "els", NA, "els", NA)
  )
  db_chemicals <- data.frame(
    stringsAsFactors = FALSE,
    cas_number = c(123, 987, 111),
    chemical_name = c("Ca", "ba", "Za"),
    present_in_bc_wqg = c(FALSE, TRUE, FALSE)
  )
  db_duration_unit_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("eslk", "lhv15-20", "d", "h", "wph"),
    duration_unit_description = c(
      "Early silk stage",
      "Leaf harvest, 15-20 cm", "Day(s)",
      "Hour(s)", "Weeks post-hatch"
    ),
    duration_units_to_keep = c(FALSE, FALSE, TRUE, TRUE, TRUE),
    duration_value_multiplier_to_hours = c(NA, NA, 24, 1, 168)
  )
  db_concentration_unit_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c(
      "acts/3 mi", "ml/100 g bdwt",
      "AI ppm H2O", "ug/L", "mg/L"
    ),
    concentration_unit_description = c(
      "acts per 3 minutes",
      "milliliter per 100 grams body weight",
      "active ingredient parts per million water",
      "micrograms per liter", "miligrams per liter"
    ),
    conc_conversion_flag = c(NA, NA, NA, TRUE, TRUE),
    conc_conversion_value_multiplier = c(NA, NA, NA, 0.001, 1),
    conc_conversion_unit = c(NA, NA, NA, "mg/L", "mg/L")
  )
  db_references <- data.frame(
    stringsAsFactors = FALSE,
    reference_number = c(2L, 4L),
    author = c("A. Smith", "B. Apples"),
    title = c("Reference title 1", "Reference title 2"),
    source = c("Journ. App. Chem.", "Journ. Bio"),
    publication_year = c("1967", "1991")
  )
  db_effect_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "ACC", "POP"),
    effect_description = c("Unspecified", "Accumulation", "Population")
  )
  db_media_type_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "FW", "SW"),
    media_description = c("Unspecified", "Fresh water", "Salt water"),
    media_type_group = c("not reported", "fresh water", "salt water")
  )
  db_meta_data_download <- data.frame(
    stringsAsFactors = FALSE,
    download_date = c("2023-03-30 15:20:20"),
    version = c(" ecotox_ascii_09_15_2022")
  )

  data <- wqbench:::join_data(
    db_results = db_results,
    db_tests = db_tests,
    db_endpoint_codes = db_endpoint_codes,
    db_species = db_species,
    db_lifestage_codes = db_lifestage_codes,
    db_chemicals = db_chemicals,
    db_duration_unit_codes = db_duration_unit_codes,
    db_concentration_unit_codes = db_concentration_unit_codes,
    db_references = db_references,
    db_effect_codes = db_effect_codes,
    db_media_type_codes = db_media_type_codes,
    db_meta_data_download = db_meta_data_download
  )
  expect_equal(
    data$duration_unit,
    c("d", "d", "d", "lhv15-20", "d", "h")
  )
  expect_equal(
    data$duration_units_to_keep,
    c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE)
  )
  expect_equal(
    data$duration_value_multiplier_to_hours,
    c(24, 24, 24, NA_real_, 24, 1)
  )
})

test_that("conc codes are joined properly to conc unit", {
  db_results <- data.frame(
    stringsAsFactors = FALSE,
    test_id = c(1L, 1L, 2L, 3L, 4L, 5L),
    result_id = c(4L, 5L, 7L, 8L, 9L, 10L),
    endpoint = c("LC50", "LC50", "LC50", "LC50", "LC50", "LC50"),
    effect = c("MOR", "MOR", "MOR", "MOR", "MOR", "MOR"),
    conc1_mean = c("1000", "1", "10", "0.1", "1", "5"),
    conc1_unit = c("acts/3 mi", "ug/L", "mg/L", "ml/100 g bdwt", "ug/L", "mg/L"),
    conc2_mean = c(NA, NA, NA, NA, NA, NA),
    conc2_unit = c(NA, NA, NA, NA, NA, NA),
    conc3_mean = c(NA, NA, NA, NA, NA, NA),
    conc3_unit = c(NA, NA, NA, NA, NA, NA),
    obs_duration_mean = c("1", "1", "1", "1", "1", "1"),
    obs_duration_unit = c("h", "h", "h", "h", "h", "h"),
    additional_comments_results = c(" ", " ", " ", " ", " ", " ")
  )
  db_tests <- data.frame(
    stringsAsFactors = FALSE,
    test_cas = c(987, 123, 123, 111, 111),
    test_id = c(1L, 2L, 3L, 4L, 5L),
    study_duration_mean = c(NA, NA, NA, NA, NA),
    study_duration_unit = c(NA, NA, NA, NA, NA),
    organism_habitat = c("Water", "Water", "Water", "Water", "Water"),
    species_number = c(1L, 2L, 4L, 3L, 2L),
    media_type = c("FW", "FW", "FW", "FW", "FW"),
    organism_lifestage = c("AD", "AD", "AD", "AD", "AD"),
    reference_number = c(2L, 2L, 2L, 2L, 2L),
    additional_comments_tests = c(" ", " ", " ", " ", " ")
  )
  db_endpoint_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("MATC", "LC50", "LC50*", "--", "AC50"),
    description = c(
      "", "Lethal concentration to 50% of test organisms",
      "Lethal concentration to 50% of test organisms",
      "Unspecified",
      "The concentration corresponding to 50% maximal activity. Used in in vitro testing."
    ),
    concentration_flag = c(TRUE, TRUE, TRUE, FALSE, FALSE)
  )
  db_species <- data.frame(
    stringsAsFactors = FALSE,
    species_number = c(1L, 2L, 3L, 4L),
    latin_name = c(
      "Pimephales promelas", "Lepomis macrochirus", "Hyalella azteca",
      "Orconectes nais"
    ),
    common_name = c("Fathead Minnow", "Bluegill", "Scud", "Crayfish"),
    kingdom = c("Animalia", "Animalia", "Animalia", "Animalia"),
    phylum_division = c("Chordata", "Chordata", "Arthropoda", "Arthropoda"),
    subphylum_div = c("Vertebrata", "Vertebrata", "Crustacea", "Crustacea"),
    superclass = c("Osteichthyes", "Osteichthyes", NA, NA),
    class = c("Actinopterygii", "Actinopterygii", "Malacostraca", "Malacostraca"),
    tax_order = c("Cypriniformes", "Perciformes", "Amphipoda", "Decapoda"),
    family = c("Cyprinidae", "Centrarchidae", "Hyalellidae", "Cambaridae"),
    genus = c("Pimephales", "Lepomis", "Hyalella", "Orconectes"),
    species = c("promelas", "macrochirus", "azteca", "nais"),
    subspecies = c(NA, NA, NA, NA),
    variety = c(NA, NA, NA, NA),
    ecotox_group = c(NA, "Fish", NA, NA),
    species_present_in_bc = c(TRUE, TRUE, FALSE, FALSE),
    ecological_group = c("Other", "Salmonid", "Other", "Planktonic Invertebrate"),
    trophic_group = c("Fish", "Fish", "Invertebrate", "Invertebrate")
  )
  db_lifestage_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "AD", "AL", "BD", "BL", "BS"),
    lifestage_description = c(
      "Unspecified", "Adult", "Alevin", "Bud or Budding",
      "Blastula", "Bud blast stage"
    ),
    simple_lifestage = c("adult", "adult", "els", NA, "els", NA)
  )
  db_chemicals <- data.frame(
    stringsAsFactors = FALSE,
    cas_number = c(123, 987, 111),
    chemical_name = c("Ca", "ba", "Za"),
    present_in_bc_wqg = c(FALSE, TRUE, FALSE)
  )
  db_duration_unit_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("eslk", "lhv15-20", "d", "h", "wph"),
    duration_unit_description = c(
      "Early silk stage",
      "Leaf harvest, 15-20 cm", "Day(s)",
      "Hour(s)", "Weeks post-hatch"
    ),
    duration_units_to_keep = c(FALSE, FALSE, TRUE, TRUE, TRUE),
    duration_value_multiplier_to_hours = c(NA, NA, 24, 1, 168)
  )
  db_concentration_unit_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c(
      "acts/3 mi", "ml/100 g bdwt",
      "AI ppm H2O", "ug/L", "mg/L"
    ),
    concentration_unit_description = c(
      "acts per 3 minutes",
      "milliliter per 100 grams body weight",
      "active ingredient parts per million water",
      "micrograms per liter", "miligrams per liter"
    ),
    conc_conversion_flag = c(NA, NA, NA, TRUE, TRUE),
    conc_conversion_value_multiplier = c(NA, NA, NA, 0.001, 1),
    conc_conversion_unit = c(NA, NA, NA, "mg/L", "mg/L")
  )
  db_references <- data.frame(
    stringsAsFactors = FALSE,
    reference_number = c(2L, 4L),
    author = c("A. Smith", "B. Apples"),
    title = c("Reference title 1", "Reference title 2"),
    source = c("Journ. App. Chem.", "Journ. Bio"),
    publication_year = c("1967", "1991")
  )
  db_effect_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "ACC", "POP"),
    effect_description = c("Unspecified", "Accumulation", "Population")
  )
  db_media_type_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "FW", "SW"),
    media_description = c("Unspecified", "Fresh water", "Salt water"),
    media_type_group = c("not reported", "fresh water", "salt water")
  )
  db_meta_data_download <- data.frame(
    stringsAsFactors = FALSE,
    download_date = c("2023-03-30 15:20:20"),
    version = c(" ecotox_ascii_09_15_2022")
  )

  data <- wqbench:::join_data(
    db_results = db_results,
    db_tests = db_tests,
    db_endpoint_codes = db_endpoint_codes,
    db_species = db_species,
    db_lifestage_codes = db_lifestage_codes,
    db_chemicals = db_chemicals,
    db_duration_unit_codes = db_duration_unit_codes,
    db_concentration_unit_codes = db_concentration_unit_codes,
    db_references = db_references,
    db_effect_codes = db_effect_codes,
    db_media_type_codes = db_media_type_codes,
    db_meta_data_download = db_meta_data_download
  )
  expect_equal(
    data$conc1_unit,
    c("acts/3 mi", "ug/L", "mg/L", "ml/100 g bdwt", "ug/L", "mg/L")
  )
  expect_equal(
    data$conc_conversion_flag,
    c(NA, TRUE, TRUE, NA, TRUE, TRUE)
  )
  expect_equal(
    data$conc_conversion_value_multiplier,
    c(NA_real_, 0.001, 1, NA, 0.001, 1)
  )
})

test_that("references are joined properly", {
  db_results <- data.frame(
    stringsAsFactors = FALSE,
    test_id = c(1L, 1L, 2L, 3L, 4L, 5L),
    result_id = c(4L, 5L, 7L, 8L, 9L, 10L),
    endpoint = c("LC50", "LC50", "LC50", "LC50", "LC50", "LC50"),
    effect = c("MOR", "MOR", "MOR", "MOR", "MOR", "MOR"),
    conc1_mean = c("1", "1", "1", "1", "1", "1"),
    conc1_unit = c("mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L"),
    conc2_mean = c(NA, NA, NA, NA, NA, NA),
    conc2_unit = c(NA, NA, NA, NA, NA, NA),
    conc3_mean = c(NA, NA, NA, NA, NA, NA),
    conc3_unit = c(NA, NA, NA, NA, NA, NA),
    obs_duration_mean = c("1", "1", "1", "1", "1", "1"),
    obs_duration_unit = c("h", "h", "h", "h", "h", "h"),
    additional_comments_results = c(" ", " ", " ", " ", " ", " ")
  )
  db_tests <- data.frame(
    stringsAsFactors = FALSE,
    test_cas = c(987, 123, 123, 111, 111),
    test_id = c(1L, 2L, 3L, 4L, 5L),
    study_duration_mean = c(NA, NA, NA, NA, NA),
    study_duration_unit = c(NA, NA, NA, NA, NA),
    organism_habitat = c("Water", "Water", "Water", "Water", "Water"),
    species_number = c(1L, 2L, 4L, 3L, 2L),
    media_type = c("FW", "FW", "FW", "FW", "FW"),
    organism_lifestage = c("AD", "AD", "AD", "AD", "AD"),
    reference_number = c(2L, 1L, 2L, 3L, 2L),
    additional_comments_tests = c(" ", " ", " ", " ", " ")
  )
  db_endpoint_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("MATC", "LC50", "LC50*", "--", "AC50"),
    description = c(
      "", "Lethal concentration to 50% of test organisms",
      "Lethal concentration to 50% of test organisms",
      "Unspecified",
      "The concentration corresponding to 50% maximal activity. Used in in vitro testing."
    ),
    concentration_flag = c(TRUE, TRUE, TRUE, FALSE, FALSE)
  )
  db_species <- data.frame(
    stringsAsFactors = FALSE,
    species_number = c(1L, 2L, 3L, 4L),
    latin_name = c(
      "Pimephales promelas", "Lepomis macrochirus", "Hyalella azteca",
      "Orconectes nais"
    ),
    common_name = c("Fathead Minnow", "Bluegill", "Scud", "Crayfish"),
    kingdom = c("Animalia", "Animalia", "Animalia", "Animalia"),
    phylum_division = c("Chordata", "Chordata", "Arthropoda", "Arthropoda"),
    subphylum_div = c("Vertebrata", "Vertebrata", "Crustacea", "Crustacea"),
    superclass = c("Osteichthyes", "Osteichthyes", NA, NA),
    class = c("Actinopterygii", "Actinopterygii", "Malacostraca", "Malacostraca"),
    tax_order = c("Cypriniformes", "Perciformes", "Amphipoda", "Decapoda"),
    family = c("Cyprinidae", "Centrarchidae", "Hyalellidae", "Cambaridae"),
    genus = c("Pimephales", "Lepomis", "Hyalella", "Orconectes"),
    species = c("promelas", "macrochirus", "azteca", "nais"),
    subspecies = c(NA, NA, NA, NA),
    variety = c(NA, NA, NA, NA),
    ecotox_group = c(NA, "Fish", NA, NA),
    species_present_in_bc = c(TRUE, TRUE, FALSE, FALSE),
    ecological_group = c("Other", "Salmonid", "Other", "Planktonic Invertebrate"),
    trophic_group = c("Fish", "Fish", "Invertebrate", "Invertebrate")
  )
  db_lifestage_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "AD", "AL", "BD", "BL", "BS"),
    lifestage_description = c(
      "Unspecified", "Adult", "Alevin", "Bud or Budding",
      "Blastula", "Bud blast stage"
    ),
    simple_lifestage = c("adult", "adult", "els", NA, "els", NA)
  )
  db_chemicals <- data.frame(
    stringsAsFactors = FALSE,
    cas_number = c(123, 987, 111),
    chemical_name = c("Ca", "ba", "Za"),
    present_in_bc_wqg = c(FALSE, TRUE, FALSE)
  )
  db_duration_unit_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("eslk", "lhv15-20", "d", "h", "wph"),
    duration_unit_description = c(
      "Early silk stage",
      "Leaf harvest, 15-20 cm", "Day(s)",
      "Hour(s)", "Weeks post-hatch"
    ),
    duration_units_to_keep = c(FALSE, FALSE, TRUE, TRUE, TRUE),
    duration_value_multiplier_to_hours = c(NA, NA, 24, 1, 168)
  )
  db_concentration_unit_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c(
      "acts/3 mi", "ml/100 g bdwt",
      "AI ppm H2O", "ug/L", "mg/L"
    ),
    concentration_unit_description = c(
      "acts per 3 minutes",
      "milliliter per 100 grams body weight",
      "active ingredient parts per million water",
      "micrograms per liter", "miligrams per liter"
    ),
    conc_conversion_flag = c(NA, NA, NA, TRUE, TRUE),
    conc_conversion_value_multiplier = c(NA, NA, NA, 0.001, 1),
    conc_conversion_unit = c(NA, NA, NA, "mg/L", "mg/L")
  )
  db_references <- data.frame(
    stringsAsFactors = FALSE,
    reference_number = c(2L, 3L),
    author = c("A. Smith", "B. Apples"),
    title = c("Reference title 1", "Reference title 2"),
    source = c("Journ. App. Chem.", "Journ. Bio"),
    publication_year = c("1967", "1991")
  )
  db_effect_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "ACC", "POP"),
    effect_description = c("Unspecified", "Accumulation", "Population")
  )
  db_media_type_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "FW", "SW"),
    media_description = c("Unspecified", "Fresh water", "Salt water"),
    media_type_group = c("not reported", "fresh water", "salt water")
  )
  db_meta_data_download <- data.frame(
    stringsAsFactors = FALSE,
    download_date = c("2023-03-30 15:20:20"),
    version = c(" ecotox_ascii_09_15_2022")
  )

  data <- wqbench:::join_data(
    db_results = db_results,
    db_tests = db_tests,
    db_endpoint_codes = db_endpoint_codes,
    db_species = db_species,
    db_lifestage_codes = db_lifestage_codes,
    db_chemicals = db_chemicals,
    db_duration_unit_codes = db_duration_unit_codes,
    db_concentration_unit_codes = db_concentration_unit_codes,
    db_references = db_references,
    db_effect_codes = db_effect_codes,
    db_media_type_codes = db_media_type_codes,
    db_meta_data_download = db_meta_data_download
  )
  expect_equal(
    data$reference_number,
    c(2, 2, 1, 2, 3, 2)
  )
  expect_equal(
    data$author,
    c("A. Smith", "A. Smith", NA, "A. Smith", "B. Apples", "A. Smith")
  )
})

test_that("effect codes joined properly", {
  db_results <- data.frame(
    stringsAsFactors = FALSE,
    test_id = c(1L, 1L, 2L, 3L, 4L, 5L),
    result_id = c(4L, 5L, 7L, 8L, 9L, 10L),
    endpoint = c("LC50", "LC50", "LC50", "LC50", "LC50", "LC50"),
    effect = c("MOR", "GEN", "MOR", "--", "BIO", "MOR"),
    conc1_mean = c("1", "1", "1", "1", "1", "1"),
    conc1_unit = c("mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L"),
    conc2_mean = c(NA, NA, NA, NA, NA, NA),
    conc2_unit = c(NA, NA, NA, NA, NA, NA),
    conc3_mean = c(NA, NA, NA, NA, NA, NA),
    conc3_unit = c(NA, NA, NA, NA, NA, NA),
    obs_duration_mean = c("1", "1", "1", "1", "1", "1"),
    obs_duration_unit = c("h", "h", "h", "h", "h", "h"),
    additional_comments_results = c(" ", " ", " ", " ", " ", " ")
  )
  db_tests <- data.frame(
    stringsAsFactors = FALSE,
    test_cas = c(987, 123, 123, 111, 111),
    test_id = c(1L, 2L, 3L, 4L, 5L),
    study_duration_mean = c(NA, NA, NA, NA, NA),
    study_duration_unit = c(NA, NA, NA, NA, NA),
    organism_habitat = c("Water", "Water", "Water", "Water", "Water"),
    species_number = c(1L, 2L, 4L, 3L, 2L),
    media_type = c("FW", "FW", "FW", "FW", "FW"),
    organism_lifestage = c("AD", "AD", "AD", "AD", "AD"),
    reference_number = c(2L, 1L, 2L, 3L, 2L),
    additional_comments_tests = c(" ", " ", " ", " ", " ")
  )
  db_endpoint_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("MATC", "LC50", "LC50*", "--", "AC50"),
    description = c(
      "", "Lethal concentration to 50% of test organisms",
      "Lethal concentration to 50% of test organisms",
      "Unspecified",
      "The concentration corresponding to 50% maximal activity. Used in in vitro testing."
    ),
    concentration_flag = c(TRUE, TRUE, TRUE, FALSE, FALSE)
  )
  db_species <- data.frame(
    stringsAsFactors = FALSE,
    species_number = c(1L, 2L, 3L, 4L),
    latin_name = c(
      "Pimephales promelas", "Lepomis macrochirus", "Hyalella azteca",
      "Orconectes nais"
    ),
    common_name = c("Fathead Minnow", "Bluegill", "Scud", "Crayfish"),
    kingdom = c("Animalia", "Animalia", "Animalia", "Animalia"),
    phylum_division = c("Chordata", "Chordata", "Arthropoda", "Arthropoda"),
    subphylum_div = c("Vertebrata", "Vertebrata", "Crustacea", "Crustacea"),
    superclass = c("Osteichthyes", "Osteichthyes", NA, NA),
    class = c("Actinopterygii", "Actinopterygii", "Malacostraca", "Malacostraca"),
    tax_order = c("Cypriniformes", "Perciformes", "Amphipoda", "Decapoda"),
    family = c("Cyprinidae", "Centrarchidae", "Hyalellidae", "Cambaridae"),
    genus = c("Pimephales", "Lepomis", "Hyalella", "Orconectes"),
    species = c("promelas", "macrochirus", "azteca", "nais"),
    subspecies = c(NA, NA, NA, NA),
    variety = c(NA, NA, NA, NA),
    ecotox_group = c(NA, "Fish", NA, NA),
    species_present_in_bc = c(TRUE, TRUE, FALSE, FALSE),
    ecological_group = c("Other", "Salmonid", "Other", "Planktonic Invertebrate"),
    trophic_group = c("Fish", "Fish", "Invertebrate", "Invertebrate")
  )
  db_lifestage_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "AD", "AL", "BD", "BL", "BS"),
    lifestage_description = c(
      "Unspecified", "Adult", "Alevin", "Bud or Budding",
      "Blastula", "Bud blast stage"
    ),
    simple_lifestage = c("adult", "adult", "els", NA, "els", NA)
  )
  db_chemicals <- data.frame(
    stringsAsFactors = FALSE,
    cas_number = c(123, 987, 111),
    chemical_name = c("Ca", "ba", "Za"),
    present_in_bc_wqg = c(FALSE, TRUE, FALSE)
  )
  db_duration_unit_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("eslk", "lhv15-20", "d", "h", "wph"),
    duration_unit_description = c(
      "Early silk stage",
      "Leaf harvest, 15-20 cm", "Day(s)",
      "Hour(s)", "Weeks post-hatch"
    ),
    duration_units_to_keep = c(FALSE, FALSE, TRUE, TRUE, TRUE),
    duration_value_multiplier_to_hours = c(NA, NA, 24, 1, 168)
  )
  db_concentration_unit_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c(
      "acts/3 mi", "ml/100 g bdwt",
      "AI ppm H2O", "ug/L", "mg/L"
    ),
    concentration_unit_description = c(
      "acts per 3 minutes",
      "milliliter per 100 grams body weight",
      "active ingredient parts per million water",
      "micrograms per liter", "miligrams per liter"
    ),
    conc_conversion_flag = c(NA, NA, NA, TRUE, TRUE),
    conc_conversion_value_multiplier = c(NA, NA, NA, 0.001, 1),
    conc_conversion_unit = c(NA, NA, NA, "mg/L", "mg/L")
  )
  db_references <- data.frame(
    stringsAsFactors = FALSE,
    reference_number = c(2L, 3L),
    author = c("A. Smith", "B. Apples"),
    title = c("Reference title 1", "Reference title 2"),
    source = c("Journ. App. Chem.", "Journ. Bio"),
    publication_year = c("1967", "1991")
  )
  db_effect_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "MOR", "BIO", "GEN"),
    effect_description = c("Unspecified", "Mortality", "Biochemistry", "Genetics")
  )
  db_media_type_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "FW", "SW"),
    media_description = c("Unspecified", "Fresh water", "Salt water"),
    media_type_group = c("not reported", "fresh water", "salt water")
  )
  db_meta_data_download <- data.frame(
    stringsAsFactors = FALSE,
    download_date = c("2023-03-30 15:20:20"),
    version = c(" ecotox_ascii_09_15_2022")
  )

  data <- wqbench:::join_data(
    db_results = db_results,
    db_tests = db_tests,
    db_endpoint_codes = db_endpoint_codes,
    db_species = db_species,
    db_lifestage_codes = db_lifestage_codes,
    db_chemicals = db_chemicals,
    db_duration_unit_codes = db_duration_unit_codes,
    db_concentration_unit_codes = db_concentration_unit_codes,
    db_references = db_references,
    db_effect_codes = db_effect_codes,
    db_media_type_codes = db_media_type_codes,
    db_meta_data_download = db_meta_data_download
  )
  expect_equal(
    data$effect_description,
    c("Mortality", "Genetics", "Mortality", "Unspecified", "Biochemistry", "Mortality")
  )
  expect_equal(
    data$effect,
    c("MOR", "GEN", "MOR", "--", "BIO", "MOR")
  )
})

test_that("media codes joined properly", {
  db_results <- data.frame(
    stringsAsFactors = FALSE,
    test_id = c(1L, 1L, 2L, 3L, 4L, 5L),
    result_id = c(4L, 5L, 7L, 8L, 9L, 10L),
    endpoint = c("LC50", "LC50", "LC50", "LC50", "LC50", "LC50"),
    effect = c("MOR", "MOR", "MOR", "MOR", "MOR", "MOR"),
    conc1_mean = c("1", "1", "1", "1", "1", "1"),
    conc1_unit = c("mg/L", "mg/L", "mg/L", "mg/L", "mg/L", "mg/L"),
    conc2_mean = c(NA, NA, NA, NA, NA, NA),
    conc2_unit = c(NA, NA, NA, NA, NA, NA),
    conc3_mean = c(NA, NA, NA, NA, NA, NA),
    conc3_unit = c(NA, NA, NA, NA, NA, NA),
    obs_duration_mean = c("1", "1", "1", "1", "1", "1"),
    obs_duration_unit = c("h", "h", "h", "h", "h", "h"),
    additional_comments_results = c(" ", " ", " ", " ", " ", " ")
  )
  db_tests <- data.frame(
    stringsAsFactors = FALSE,
    test_cas = c(987, 123, 123, 111, 111),
    test_id = c(1L, 2L, 3L, 4L, 5L),
    study_duration_mean = c(NA, NA, NA, NA, NA),
    study_duration_unit = c(NA, NA, NA, NA, NA),
    organism_habitat = c("Water", "Water", "Water", "Water", "Water"),
    species_number = c(1L, 2L, 4L, 3L, 2L),
    media_type = c("FW", "SW/", "--", "FW/", "SW"),
    organism_lifestage = c("AD", "AD", "AD", "AD", "AD"),
    reference_number = c(2L, 1L, 2L, 3L, 2L),
    additional_comments_tests = c(" ", " ", " ", " ", " ")
  )
  db_endpoint_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("MATC", "LC50", "LC50*", "--", "AC50"),
    description = c(
      "", "Lethal concentration to 50% of test organisms",
      "Lethal concentration to 50% of test organisms",
      "Unspecified",
      "The concentration corresponding to 50% maximal activity. Used in in vitro testing."
    ),
    concentration_flag = c(TRUE, TRUE, TRUE, FALSE, FALSE)
  )
  db_species <- data.frame(
    stringsAsFactors = FALSE,
    species_number = c(1L, 2L, 3L, 4L),
    latin_name = c(
      "Pimephales promelas", "Lepomis macrochirus", "Hyalella azteca",
      "Orconectes nais"
    ),
    common_name = c("Fathead Minnow", "Bluegill", "Scud", "Crayfish"),
    kingdom = c("Animalia", "Animalia", "Animalia", "Animalia"),
    phylum_division = c("Chordata", "Chordata", "Arthropoda", "Arthropoda"),
    subphylum_div = c("Vertebrata", "Vertebrata", "Crustacea", "Crustacea"),
    superclass = c("Osteichthyes", "Osteichthyes", NA, NA),
    class = c("Actinopterygii", "Actinopterygii", "Malacostraca", "Malacostraca"),
    tax_order = c("Cypriniformes", "Perciformes", "Amphipoda", "Decapoda"),
    family = c("Cyprinidae", "Centrarchidae", "Hyalellidae", "Cambaridae"),
    genus = c("Pimephales", "Lepomis", "Hyalella", "Orconectes"),
    species = c("promelas", "macrochirus", "azteca", "nais"),
    subspecies = c(NA, NA, NA, NA),
    variety = c(NA, NA, NA, NA),
    ecotox_group = c(NA, "Fish", NA, NA),
    species_present_in_bc = c(TRUE, TRUE, FALSE, FALSE),
    ecological_group = c("Other", "Salmonid", "Other", "Planktonic Invertebrate"),
    trophic_group = c("Fish", "Fish", "Invertebrate", "Invertebrate")
  )
  db_lifestage_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "AD", "AL", "BD", "BL", "BS"),
    lifestage_description = c(
      "Unspecified", "Adult", "Alevin", "Bud or Budding",
      "Blastula", "Bud blast stage"
    ),
    simple_lifestage = c("adult", "adult", "els", NA, "els", NA)
  )
  db_chemicals <- data.frame(
    stringsAsFactors = FALSE,
    cas_number = c(123, 987, 111),
    chemical_name = c("Ca", "ba", "Za"),
    present_in_bc_wqg = c(FALSE, TRUE, FALSE)
  )
  db_duration_unit_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("eslk", "lhv15-20", "d", "h", "wph"),
    duration_unit_description = c(
      "Early silk stage",
      "Leaf harvest, 15-20 cm", "Day(s)",
      "Hour(s)", "Weeks post-hatch"
    ),
    duration_units_to_keep = c(FALSE, FALSE, TRUE, TRUE, TRUE),
    duration_value_multiplier_to_hours = c(NA, NA, 24, 1, 168)
  )
  db_concentration_unit_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c(
      "acts/3 mi", "ml/100 g bdwt",
      "AI ppm H2O", "ug/L", "mg/L"
    ),
    concentration_unit_description = c(
      "acts per 3 minutes",
      "milliliter per 100 grams body weight",
      "active ingredient parts per million water",
      "micrograms per liter", "miligrams per liter"
    ),
    conc_conversion_flag = c(NA, NA, NA, TRUE, TRUE),
    conc_conversion_value_multiplier = c(NA, NA, NA, 0.001, 1),
    conc_conversion_unit = c(NA, NA, NA, "mg/L", "mg/L")
  )
  db_references <- data.frame(
    stringsAsFactors = FALSE,
    reference_number = c(2L, 3L),
    author = c("A. Smith", "B. Apples"),
    title = c("Reference title 1", "Reference title 2"),
    source = c("Journ. App. Chem.", "Journ. Bio"),
    publication_year = c("1967", "1991")
  )
  db_effect_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "MOR", "BIO", "GEN"),
    effect_description = c("Unspecified", "Mortality", "Biochemistry", "Genetics")
  )
  db_media_type_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "FW", "SW"),
    media_description = c("Unspecified", "Fresh water", "Salt water"),
    media_type_group = c("not reported", "fresh water", "salt water")
  )
  db_meta_data_download <- data.frame(
    stringsAsFactors = FALSE,
    download_date = c("2023-03-30 15:20:20"),
    version = c(" ecotox_ascii_09_15_2022")
  )

  data <- wqbench:::join_data(
    db_results = db_results,
    db_tests = db_tests,
    db_endpoint_codes = db_endpoint_codes,
    db_species = db_species,
    db_lifestage_codes = db_lifestage_codes,
    db_chemicals = db_chemicals,
    db_duration_unit_codes = db_duration_unit_codes,
    db_concentration_unit_codes = db_concentration_unit_codes,
    db_references = db_references,
    db_effect_codes = db_effect_codes,
    db_media_type_codes = db_media_type_codes,
    db_meta_data_download = db_meta_data_download
  )
  expect_equal(
    data$media_type,
    c("FW", "FW", "SW", "--", "FW", "SW")
  )
  expect_equal(
    data$media_description,
    c("Fresh water", "Fresh water", "Salt water", "Unspecified", "Fresh water", "Salt water")
  )
  expect_equal(
    data$media_type_group,
    c("fresh water", "fresh water", "salt water", "not reported", "fresh water", "salt water")
  )
})

test_that("Fungi ecotox_group is filtered out", {
  db_results <- data.frame(
    stringsAsFactors = FALSE,
    test_id = c(1L, 2L, 2L, 2L, 2L),
    result_id = c(4L, 5L, 7L, 8L, 9L),
    endpoint = c("LC50", "LC50", "LC50", "LC50", "LC50"),
    effect = c("MOR", "MOR", "MOR", "MOR", "MOR"),
    conc1_mean = c("10", "0.5", "1", "1.5", "2.0"),
    conc1_unit = c("mg/L", "mg/L", "mg/L", "mg/L", "mg/L"),
    conc2_mean = c(NA, NA, NA, NA, NA),
    conc2_unit = c(NA, NA, NA, NA, NA),
    conc3_mean = c(NA, NA, NA, NA, NA),
    conc3_unit = c(NA, NA, NA, NA, NA),
    obs_duration_mean = c("1", "1", "1", "1", "1"),
    obs_duration_unit = c("d", "d", "d", "d", "d"),
    additional_comments_results = c(" ", " ", " ", " ", " ")
  )
  
  db_tests <- data.frame(
    stringsAsFactors = FALSE,
    test_cas = c(987, 123, 123),
    test_id = c(1L, 2L, 3L),
    study_duration_mean = c("4", "4", "4"),
    study_duration_unit = c("d", "d", "d"),
    organism_habitat = c("Water", "Water", "Soil"),
    species_number = c(1L, 2L, 2L),
    media_type = c("FW", "FW", "FW"),
    organism_lifestage = c("AD", "AD", "AD"),
    reference_number = c(2L, 2L, 2L),
    additional_comments_tests = c(" ", " ", " ")
  )
  
  db_endpoint_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("MATC", "LC50", "LC50*", "--", "AC50"),
    description = c(
      "", "Lethal concentration to 50% of test organisms",
      "Lethal concentration to 50% of test organisms",
      "Unspecified",
      "The concentration corresponding to 50% maximal activity. Used in in vitro testing."
    ),
    concentration_flag = c(TRUE, TRUE, TRUE, FALSE, FALSE)
  )
  
  db_species <- data.frame(
    stringsAsFactors = FALSE,
    species_number = c(1L, 2L),
    latin_name = c("Pimephales promelas", "Lepomis macrochirus"),
    common_name = c("Fathead Minnow", "Bluegill"),
    kingdom = c("Animalia", "Animalia"),
    phylum_division = c("Chordata", "Chordata"),
    subphylum_div = c("Vertebrata", "Vertebrata"),
    superclass = c("Osteichthyes", "Osteichthyes"),
    class = c("Actinopterygii", "Actinopterygii"),
    tax_order = c("Cypriniformes", "Perciformes"),
    family = c("Cyprinidae", "Centrarchidae"),
    genus = c("Pimephales", "Lepomis"),
    species = c("promelas", "macrochirus"),
    subspecies = c(NA, NA),
    variety = c(NA, NA),
    ecotox_group = c("Fungi", "Fish"),
    species_present_in_bc = c(TRUE, TRUE),
    ecological_group = c("Other", "Salmonid"),
    trophic_group = c("Invertebrate", "Fish")
  )
  
  db_lifestage_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "AD", "AL", "BD", "BL", "BS"),
    lifestage_description = c(
      "Unspecified", "Adult", "Alevin", "Bud or Budding",
      "Blastula", "Bud blast stage"
    ),
    simple_lifestage = c("adult", "adult", "els", NA, "els", NA)
  )
  
  db_chemicals <- data.frame(
    stringsAsFactors = FALSE,
    cas_number = c(123, 987, 111),
    chemical_name = c("Ca", "ba", "Za"),
    present_in_bc_wqg = c(FALSE, TRUE, FALSE)
  )
  
  db_duration_unit_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("eslk", "lhv15-20", "d", "h", "wph"),
    duration_unit_description = c(
      "Early silk stage",
      "Leaf harvest, 15-20 cm", "Day(s)",
      "Hour(s)", "Weeks post-hatch"
    ),
    duration_units_to_keep = c(FALSE, FALSE, FALSE, TRUE, TRUE),
    duration_value_multiplier_to_hours = c(NA, NA, 24, 1, 168)
  )
  
  db_concentration_unit_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c(
      "acts/3 mi", "ml/100 g bdwt",
      "AI ppm H2O", "ug/L", "mg/L"
    ),
    concentration_unit_description = c(
      "acts per 3 minutes",
      "milliliter per 100 grams body weight",
      "active ingredient parts per million water",
      "micrograms per liter", "miligrams per liter"
    ),
    conc_conversion_flag = c(NA, NA, NA, TRUE, TRUE),
    conc_conversion_value_multiplier = c(NA, NA, NA, 0.001, 1),
    conc_conversion_unit = c(NA, NA, NA, "mg/L", "mg/L")
  )
  
  db_references <- data.frame(
    stringsAsFactors = FALSE,
    reference_number = c(2L, 4L),
    author = c("A. Smith", "B. Apples"),
    title = c("Reference title 1", "Reference title 2"),
    source = c("Journ. App. Chem.", "Journ. Bio"),
    publication_year = c("1967", "1991")
  )
  
  db_effect_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "ACC", "POP"),
    effect_description = c("Unspecified", "Accumulation", "Population")
  )
  
  db_media_type_codes <- data.frame(
    stringsAsFactors = FALSE,
    code = c("--", "FW", "SW"),
    media_description = c("Unspecified", "Fresh water", "Salt water"),
    media_type_group = c("not reported", "fresh water", "salt water")
  )
  
  db_meta_data_download <- data.frame(
    stringsAsFactors = FALSE,
    download_date = c("2023-03-30 15:20:20"),
    version = c(" ecotox_ascii_09_15_2022")
  )
  
  data <- wqbench:::join_data(
    db_results = db_results,
    db_tests = db_tests,
    db_endpoint_codes = db_endpoint_codes,
    db_species = db_species,
    db_lifestage_codes = db_lifestage_codes,
    db_chemicals = db_chemicals,
    db_duration_unit_codes = db_duration_unit_codes,
    db_concentration_unit_codes = db_concentration_unit_codes,
    db_references = db_references,
    db_effect_codes = db_effect_codes,
    db_media_type_codes = db_media_type_codes,
    db_meta_data_download = db_meta_data_download
  )
  
  expect_equal(
    nrow(data),
    4L
  )
})
