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

test_that("data passes with single row of data", {
  data <- data.frame(
    latin_name = c("a"),
    endpoint = c("NOEL"),
    effect = c("Mortality"),
    lifestage = c("Post-spawning"),
    simple_lifestage = c("adult"),
    effect_conc_mg.L = c(1.1),
    duration_hrs = c(1.1),
    trophic_group = c("Plant"),
    ecological_group = c("Other"),
    species_present_in_bc = c("TRUE")
  )
  output <- wqb_check_add_data(data, template)
  expect_equal(
    output,
    data |> dplyr::mutate(dplyr::across(species_present_in_bc, as.logical))
  )
})

test_that("data passes species_present as logical", {
  data <- data.frame(
    latin_name = c("a"),
    endpoint = c("NOEL"),
    effect = c("Mortality"),
    lifestage = c("Post-spawning"),
    simple_lifestage = c("adult"),
    effect_conc_mg.L = c(1.1),
    duration_hrs = c(1.1),
    trophic_group = c("Plant"),
    ecological_group = c("Other"),
    species_present_in_bc = c(TRUE)
  )
  output <- wqb_check_add_data(data, template)
  expect_equal(
    output,
    data |> dplyr::mutate(dplyr::across(species_present_in_bc, as.logical))
  )
})

test_that("data passes with multiple rows of data", {
  data <- data.frame(
    latin_name = c("a", "b", "c"),
    endpoint = c("NOEL", "EC50", "lc50"),
    effect = c("Mortality", "Reproduction", "Mortality"),
    lifestage = c("Post-spawning"),
    simple_lifestage = c("adult", "els", "Juvenile"),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    duration_hrs = c(1, 2, 3),
    trophic_group = c("Fish", "plant", "Plant"),
    ecological_group = c("Salmonid", "Other", "other"),
    species_present_in_bc = c("TRUE")
  )
  output <- wqb_check_add_data(data, template)
  expect_equal(
    output,
    data |> 
      dplyr::mutate(dplyr::across(species_present_in_bc, as.logical)) |>
      dplyr::mutate(endpoint = stringr::str_to_upper(endpoint)) |>
      dplyr::mutate(trophic_group = stringr::str_to_sentence(trophic_group)) |>
      dplyr::mutate(ecological_group = stringr::str_to_sentence(ecological_group)) |>
      dplyr::mutate(simple_lifestage = stringr::str_to_lower(simple_lifestage))
  )
})

test_that("errors bad endpoint", {
  data <- data.frame(
    latin_name = c("a", "b", "c"),
    endpoint = c("NOEL", "EC50", "X2"),
    effect = c("Mortality", "Reproduction", "Mortality"),
    lifestage = c("Post-spawning"),
    simple_lifestage = c("Adult", "juvenile", "els"),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    duration_hrs = c(1, 2, 3),
    trophic_group = c("Fish", "Plant", "Plant"),
    ecological_group = c("Salmonid", "Other", "Other"),
    species_present_in_bc = c("TRUE")
  )
  expect_error(
    wqb_check_add_data(data, template),
    regexp = "The endpoint column has invalid value\\(s\\)\\. The allowed values include\\: EC05, EC06, EC07, EC08, EC09, EC10, EC11, EC12, EC13, EC14, EC15, EC16, EC17, EC18, EC19, EC20, EC21, EC22, EC23, EC24, EC25, EC26, EC27, EC28, EC29, EC30, EC31, EC32, EC33, EC34, EC35, EC36, EC37, EC38, EC39, EC40, EC41, EC42, EC43, EC44, EC45, EC46, EC47, EC48, EC49, EC50, EC51, EC52, EC53, EC54, EC55, IC05, IC06, IC07, IC08, IC09, IC10, IC11, IC12, IC13, IC14, IC15, IC16, IC17, IC18, IC19, IC20, IC21, IC22, IC23, IC24, IC25, IC26, IC27, IC28, IC29, IC30, IC31, IC32, IC33, IC34, IC35, IC36, IC37, IC38, IC39, IC40, IC41, IC42, IC43, IC44, IC45, IC46, IC47, IC48, IC49, IC50, IC51, IC52, IC53, IC54, IC55, LC05, LC06, LC07, LC08, LC09, LC10, LC11, LC12, LC13, LC14, LC15, LC16, LC17, LC18, LC19, LC20, LC21, LC22, LC23, LC24, LC25, LC26, LC27, LC28, LC29, LC30, LC31, LC32, LC33, LC34, LC35, LC36, LC37, LC38, LC39, LC40, LC41, LC42, LC43, LC44, LC45, LC46, LC47, LC48, LC49, LC50, LC51, LC52, LC53, LC54, LC55, LOEC, LOEL, MATC, MCIG, NOEL, NOEC."
  )
})

test_that("errors bad endpoint", {
  data <- data.frame(
    latin_name = c("a", "b", "c"),
    endpoint = c("NOEL", "EC50", "X2"),
    effect = c("Mortality", "Reproduction", "Mortality"),
    lifestage = c("Post-spawning"),
    simple_lifestage = c("adult", "juvenile", "els"),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    duration_hrs = c(1, 2, 3),
    trophic_group = c("Fish", "Plant", "Plant"),
    ecological_group = c("Salmonid", "Other", "Other"),
    species_present_in_bc = c("TRUE")
  )
  expect_error(
    wqb_check_add_data(data, template),
    regexp = "The endpoint column has invalid value\\(s\\)\\. The allowed values include\\: EC05, EC06, EC07, EC08, EC09, EC10, EC11, EC12, EC13, EC14, EC15, EC16, EC17, EC18, EC19, EC20, EC21, EC22, EC23, EC24, EC25, EC26, EC27, EC28, EC29, EC30, EC31, EC32, EC33, EC34, EC35, EC36, EC37, EC38, EC39, EC40, EC41, EC42, EC43, EC44, EC45, EC46, EC47, EC48, EC49, EC50, EC51, EC52, EC53, EC54, EC55, IC05, IC06, IC07, IC08, IC09, IC10, IC11, IC12, IC13, IC14, IC15, IC16, IC17, IC18, IC19, IC20, IC21, IC22, IC23, IC24, IC25, IC26, IC27, IC28, IC29, IC30, IC31, IC32, IC33, IC34, IC35, IC36, IC37, IC38, IC39, IC40, IC41, IC42, IC43, IC44, IC45, IC46, IC47, IC48, IC49, IC50, IC51, IC52, IC53, IC54, IC55, LC05, LC06, LC07, LC08, LC09, LC10, LC11, LC12, LC13, LC14, LC15, LC16, LC17, LC18, LC19, LC20, LC21, LC22, LC23, LC24, LC25, LC26, LC27, LC28, LC29, LC30, LC31, LC32, LC33, LC34, LC35, LC36, LC37, LC38, LC39, LC40, LC41, LC42, LC43, LC44, LC45, LC46, LC47, LC48, LC49, LC50, LC51, LC52, LC53, LC54, LC55, LOEC, LOEL, MATC, MCIG, NOEL, NOEC."
  )
})

test_that("errors bad trophic group", {
  data <- data.frame(
    latin_name = c("a", "b", "c"),
    endpoint = c("NOEL", "EC50", "LC34"),
    effect = c("Mortality", "Reproduction", "Mortality"),
    lifestage = c("Post-spawning"),
    simple_lifestage = c("adult", "juvenile", "els"),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    duration_hrs = c(1, 2, 3),
    trophic_group = c("XXXX", "Plant", "Plant"),
    ecological_group = c("Salmonid", "Other", "Other"),
    species_present_in_bc = c("TRUE")
  )
  expect_error(
    wqb_check_add_data(data, template),
    regexp = "The trophic_group column has invalid value\\(s\\). The allowed values include: Invertebrate, Algae, Amphibian, Bacteria, Fish, Plant."
  )
})

test_that("errors bad ecological group", {
  data <- data.frame(
    latin_name = c("a", "b", "c"),
    endpoint = c("NOEL", "EC50", "LC34"),
    effect = c("Mortality", "Reproduction", "Mortality"),
    lifestage = c("Post-spawning"),
    simple_lifestage = c("adult", "juvenile", "els"),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    duration_hrs = c(1, 2, 3),
    trophic_group = c("Invertebrate", "Plant", "Plant"),
    ecological_group = c("XXXX", "Other", "Other"),
    species_present_in_bc = c("TRUE")
  )
  expect_error(
    wqb_check_add_data(data, template),
    regexp = "The ecological_group column has invalid value\\(s\\). The allowed values include: Other, Planktonic Invertebrate, Salmonid."
  )
})

test_that("errors bad combo of trophic and ecological group", {
  data <- data.frame(
    latin_name = c("a", "b", "c"),
    endpoint = c("NOEL", "EC50", "LC34"),
    effect = c("Mortality", "Reproduction", "Mortality"),
    lifestage = c("Post-spawning"),
    simple_lifestage = c("Adult", "juvenile", "els"),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    duration_hrs = c(1, 2, 3),
    trophic_group = c("Invertebrate", "Plant", "Plant"),
    ecological_group = c("Salmonid", "Other", "Other"),
    species_present_in_bc = c("TRUE")
  )
  expect_error(
    wqb_check_add_data(data, template),
    regexp = "There is an invalid combination of the trophic_group or ecological_group columns. The allowed values include: Invertebrate & Planktonic Invertebrate, Invertebrate & Other, Algae & Other, Amphibian & Other, Bacteria & Other, Fish & Other, Plant & Other, Fish & Salmonid."
  )
})

test_that("errors bad range in the effect_conc_mg.L", {
  data <- data.frame(
    latin_name = c("a", "b", "c"),
    endpoint = c("NOEL", "EC50", "LC34"),
    effect = c("Mortality", "Reproduction", "Mortality"),
    lifestage = c("Post-spawning"),
    simple_lifestage = c("Adult", "juvenile", "els"),
    effect_conc_mg.L = c(-1.1, 1.2, 1.3),
    duration_hrs = c(1, 2, 3),
    trophic_group = c("Invertebrate", "Plant", "Plant"),
    ecological_group = c("Other", "Other", "Other"),
    species_present_in_bc = c("TRUE")
  )
  expect_error(
    wqb_check_add_data(data, template),
    regexp = "data\\$effect_conc_mg.L` must have values between 0 and 9e\\+06."
  )
  
  data <- data.frame(
    latin_name = c("a", "b", "c"),
    endpoint = c("NOEL", "EC50", "LC34"),
    effect = c("Mortality", "Reproduction", "Mortality"),
    lifestage = c("Post-spawning"),
    simple_lifestage = c("Adult", "juvenile", "els"),
    effect_conc_mg.L = c(10000000, 1.2, 1.3),
    duration_hrs = c(1, 2, 3),
    trophic_group = c("Invertebrate", "Plant", "Plant"),
    ecological_group = c("Other", "Other", "Other"),
    species_present_in_bc = c("TRUE")
  )
  expect_error(
    wqb_check_add_data(data, template),
    regexp = "data\\$effect_conc_mg.L` must have values between 0 and 9e\\+06."
  )
})

test_that("errors bad range in the duration_hrs", {
  data <- data.frame(
    latin_name = c("a", "b", "c"),
    endpoint = c("NOEL", "EC50", "LC34"),
    effect = c("Mortality", "Reproduction", "Mortality"),
    lifestage = c("Post-spawning"),
    simple_lifestage = c("Adult", "juvenile", "els"),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    duration_hrs = c(-0.1, 2, 3),
    trophic_group = c("Invertebrate", "Plant", "Plant"),
    ecological_group = c("Other", "Other", "Other"),
    species_present_in_bc = c("TRUE")
  )
  expect_error(
    wqb_check_add_data(data, template),
    regexp = "`data\\$duration_hrs` must have values between 0 and 40000\\."
  )

  data <- data.frame(
    latin_name = c("a", "b", "c"),
    endpoint = c("NOEL", "EC50", "LC34"),
    effect = c("Mortality", "Reproduction", "Mortality"),
    lifestage = c("Post-spawning"),
    simple_lifestage = c("Adult", "juvenile", "els"),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    duration_hrs = c(10000000, 2, 3),
    trophic_group = c("Invertebrate", "Plant", "Plant"),
    ecological_group = c("Other", "Other", "Other"),
    species_present_in_bc = c("TRUE")
  )
  expect_error(
    wqb_check_add_data(data, template),
    regexp = "`data\\$duration_hrs` must have values between 0 and 40000\\."
  )
})

test_that("errors bad species_present_in_bc", {
  data <- data.frame(
    latin_name = c("a", "b", "c"),
    endpoint = c("NOEL", "EC50", "LC34"),
    effect = c("Mortality", "Reproduction", "Mortality"),
    lifestage = c("Post-spawning"),
    simple_lifestage = c("Adult", "juvenile", "els"),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    duration_hrs = c(1, 2, 3),
    trophic_group = c("Invertebrate", "Plant", "Plant"),
    ecological_group = c("Other", "Other", "Other"),
    species_present_in_bc = c("TRUE", "yes", "FALSE")
  )
  expect_error(
    wqb_check_add_data(data, template),
    regexp = "The species_present_in_bc column has invalid value\\(s\\). The allowed values include: TRUE or FALSE."
  )
})

test_that("errors bad simple_lifestage", {
  data <- data.frame(
    latin_name = c("a", "b", "c"),
    endpoint = c("NOEL", "EC50", "LC34"),
    effect = c("Mortality", "Reproduction", "Mortality"),
    lifestage = c("Post-spawning"),
    simple_lifestage = c("Adult", "els", "Embryo"),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    duration_hrs = c(1, 2, 3),
    trophic_group = c("Invertebrate", "Plant", "Plant"),
    ecological_group = c("Other", "Other", "Other"),
    species_present_in_bc = c("TRUE", "TRUE", "FALSE")
  )
  expect_error(
    wqb_check_add_data(data, template),
    regexp = "The simple_lifestage column has invalid value\\(s\\)\\. The allowed values include\\: adult, els, juvenile."
  )
})

test_that("errors when missing values supplied", {
  data <- data.frame(
    latin_name = c("a", "b", NA_character_),
    endpoint = c("NOEL", "EC50", "LC34"),
    effect = c("Mortality", "Reproduction", "Mortality"),
    lifestage = c("Post-spawning"),
    simple_lifestage = c("Adult", "juvenile", "els"),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    duration_hrs = c(1, 2, 3),
    trophic_group = c("Invertebrate", "Plant", "Plant"),
    ecological_group = c("Other", "Other", "Other"),
    species_present_in_bc = c("TRUE", "TRUE", "FALSE")
  )
  expect_error(
    wqb_check_add_data(data, template),
    regexp = "`data\\$latin_name` must not have any missing values."
  )

  data <- data.frame(
    latin_name = c("a", "b", "c"),
    endpoint = c("NOEL", "EC50", NA_character_),
    effect = c("Mortality", "Reproduction", "Mortality"),
    lifestage = c("Post-spawning"),
    simple_lifestage = c("Adult", "juvenile", "els"),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    duration_hrs = c(1, 2, 3),
    trophic_group = c("Invertebrate", "Plant", "Plant"),
    ecological_group = c("Other", "Other", "Other"),
    species_present_in_bc = c("TRUE", "TRUE", "FALSE")
  )
  expect_error(
    wqb_check_add_data(data, template),
    regexp = "`data\\$endpoint` must not have any missing values."
  )

  data <- data.frame(
    latin_name = c("a", "b", "c"),
    endpoint = c("NOEL", "EC50", "LC50"),
    effect = c("Mortality", "Reproduction", NA_character_),
    lifestage = c("Post-spawning"),
    simple_lifestage = c("Adult", "juvenile", "els"),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    duration_hrs = c(1, 2, 3),
    trophic_group = c("Invertebrate", "Plant", "Plant"),
    ecological_group = c("Other", "Other", "Other"),
    species_present_in_bc = c("TRUE", "TRUE", "FALSE")
  )
  expect_error(
    wqb_check_add_data(data, template),
    regexp = "`data\\$effect` must not have any missing values."
  )

  data <- data.frame(
    latin_name = c("a", "b", "c"),
    endpoint = c("NOEL", "EC50", "LC50"),
    effect = c("Mortality", "Reproduction", "Mortality"),
    lifestage = c("Post-spawning"),
    simple_lifestage = c("Adult", "juvenile", NA_character_),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    duration_hrs = c(1, 2, 3),
    trophic_group = c("Invertebrate", "Plant", "Plant"),
    ecological_group = c("Other", "Other", "Other"),
    species_present_in_bc = c("TRUE", "TRUE", "FALSE")
  )
  expect_error(
    wqb_check_add_data(data, template),
    regexp = "`data\\$simple_lifestage` must not have any missing values."
  )
  
  data <- data.frame(
    latin_name = c("a", "b", "c"),
    endpoint = c("NOEL", "EC50", "LC50"),
    effect = c("Mortality", "Reproduction", "Mortality"),
    lifestage = c("Post-spawning"),
    simple_lifestage = c("Adult", "juvenile", "els"),
    effect_conc_mg.L = c(1.1, 1.2, NA_real_),
    duration_hrs = c(1, 2, 3),
    trophic_group = c("Invertebrate", "Plant", "Plant"),
    ecological_group = c("Other", "Other", "Other"),
    species_present_in_bc = c("TRUE", "TRUE", "FALSE")
  )
  expect_error(
    wqb_check_add_data(data, template),
    regexp = "`data\\$effect_conc_mg.L` must not have any missing values."
  )

  data <- data.frame(
    latin_name = c("a", "b", "c"),
    endpoint = c("NOEL", "EC50", "LC50"),
    effect = c("Mortality", "Reproduction", "Mortality"),
    lifestage = c("Post-spawning"),
    simple_lifestage = c("Adult", "juvenile", "els"),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    duration_hrs = c(1, 2, NA_real_),
    trophic_group = c("Invertebrate", "Plant", "Plant"),
    ecological_group = c("Other", "Other", "Other"),
    species_present_in_bc = c("TRUE", "TRUE", "FALSE")
  )
  expect_error(
    wqb_check_add_data(data, template),
    regexp = "`data\\$duration_hrs` must not have any missing values."
  )

  data <- data.frame(
    latin_name = c("a", "b", "c"),
    endpoint = c("NOEL", "EC50", "LC50"),
    effect = c("Mortality", "Reproduction", "Mortality"),
    lifestage = c("Post-spawning"),
    simple_lifestage = c("Adult", "juvenile", "els"),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    duration_hrs = c(1, 2, 3),
    trophic_group = c("Invertebrate", "Plant", NA_character_),
    ecological_group = c("Other", "Other", "Other"),
    species_present_in_bc = c("TRUE", "TRUE", "FALSE")
  )
  expect_error(
    wqb_check_add_data(data, template),
    regexp = "`data\\$trophic_group` must not have any missing values."
  )

  data <- data.frame(
    latin_name = c("a", "b", "c"),
    endpoint = c("NOEL", "EC50", "LC50"),
    effect = c("Mortality", "Reproduction", "Mortality"),
    lifestage = c("Post-spawning"),
    simple_lifestage = c("Adult", "juvenile", "els"),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    duration_hrs = c(1, 2, 3),
    trophic_group = c("Invertebrate", "Plant", "Plant"),
    ecological_group = c("Other", "Other", NA_character_),
    species_present_in_bc = c("TRUE", "TRUE", "FALSE")
  )
  expect_error(
    wqb_check_add_data(data, template),
    regexp = "`data\\$ecological_group` must not have any missing values."
  )

  data <- data.frame(
    latin_name = c("a", "b", "c"),
    endpoint = c("NOEL", "EC50", "LC50"),
    effect = c("Mortality", "Reproduction", "Mortality"),
    lifestage = c("Post-spawning"),
    simple_lifestage = c("Adult", "juvenile", "els"),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    duration_hrs = c(1, 2, 3),
    trophic_group = c("Invertebrate", "Plant", "Plant"),
    ecological_group = c("Other", "Other", "Other"),
    species_present_in_bc = c("TRUE", "FALSE", NA_character_)
  )
  expect_error(
    wqb_check_add_data(data, template),
    regexp = "`data\\$species_present_in_bc` must not have any missing values."
  )
})

test_that("errors with no data supplied", {
  expect_error(
    wqb_check_add_data(template = template),
    regexp = 'argument "data" is missing, with no default'
  )
})

test_that("errors with no template supplied", {
  data <- data.frame(
    latin_name = c("a"),
    endpoint = c("NOEL"),
    effect = c("Mortality"),
    lifestage = c("Post-spawning"),
    simple_lifestage = c("Adult"),
    effect_conc_mg.L = c(1.1),
    duration_hrs = c(1.1),
    trophic_group = c("Plant"),
    ecological_group = c("Other"),
    species_present_in_bc = c("TRUE")
  )
  expect_error(
    wqb_check_add_data(data),
    regexp = 'argument "template" is missing, with no default'
  )
})

test_that("errors with column missing", {
  data <- data.frame(
    latin_name = c("a"),
    endpoint = c("NOEL"),
    effect = c("Mortality"),
    lifestage = c("Post-spawning"),
    simple_lifestage = c("Adult"),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    duration_hrs = c(1.1),
    trophic_group = c("Plant"),
    ecological_group = c("Other")
  )
  expect_error(
    wqb_check_add_data(data, template),
    regexp = "Column names in data must include 'species_present_in_bc'."
  )
})

test_that("drops extra columns", {
  data <- data.frame(
    latin_name = c("a"),
    endpoint = c("NOEL"),
    effect = c("Mortality"),
    lifestage = c("Post-spawning"),
    simple_lifestage = c("Adult"),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    duration_hrs = c(1.1),
    trophic_group = c("Plant"),
    ecological_group = c("Other"),
    species_present_in_bc = c("TRUE"),
    cas = c("123456")
  )
  
  output <- wqb_check_add_data(data, template)
  expect_equal(
    colnames(output),
    c(
      "latin_name", "endpoint", "effect", "lifestage", "simple_lifestage", 
      "effect_conc_mg.L", "duration_hrs", "trophic_group", "ecological_group", 
      "species_present_in_bc"
    )
  )
})
