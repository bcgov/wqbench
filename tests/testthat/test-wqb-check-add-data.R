test_that("data passes with single row of data", {
  data <- data.frame(
    latin_name = c("a"),
    endpoint = c("NOEL"),
    effect = c("Mortality"),
    lifestage = c("Adult"),
    effect_conc_mg.L = c(1.1),
    effect_conc_std_mg.L = c(1.1),
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
    lifestage = c("Adult"),
    effect_conc_mg.L = c(1.1),
    effect_conc_std_mg.L = c(1.1),
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
    endpoint = c("NOEL", "EC50", "LC50"),
    effect = c("Mortality", "Reproduction", "Mortality"),
    lifestage = c("Adult", "Young adult", "Embryo"),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    effect_conc_std_mg.L = c(1, 2, 3),
    trophic_group = c("Fish", "Plant", "Plant"),
    ecological_group = c("Salmonid", "Other", "Other"),
    species_present_in_bc = c("TRUE")
  )
  output <- wqb_check_add_data(data, template)
  expect_equal(
    output,
    data |> dplyr::mutate(dplyr::across(species_present_in_bc, as.logical))
  )
})

test_that("errors bad endpoint", {
  data <- data.frame(
    latin_name = c("a", "b", "c"),
    endpoint = c("NOEL", "EC50", "X2"),
    effect = c("Mortality", "Reproduction", "Mortality"),
    lifestage = c("Adult", "Young adult", "Embryo"),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    effect_conc_std_mg.L = c(1, 2, 3),
    trophic_group = c("Fish", "Plant", "Plant"),
    ecological_group = c("Salmonid", "Other", "Other"),
    species_present_in_bc = c("TRUE")
  )
  expect_error(
    wqb_check_add_data(data, template),
    regexp = "The endpoint column has invalid value\\(s\\)\\. The allowed values include\\: \\(log\\)EC50, \\(log\\)LC50, EC05, EC08, EC10, EC12.5, EC13, EC15, EC16, EC18, EC20, EC22, EC23, EC24, EC25, EC30, EC31, EC32, EC34, EC35, EC37, EC38, EC40, EC41, EC45, EC46, EC50, EC52, EC55, EC58, IC05, IC07, IC10, IC15, IC16, IC20, IC25, IC27, IC30, IC40, IC50, LC05, LC08, LC10, LC15, LC16, LC20, LC25, LC30, LC31, LC34, LC35, LC38, LC40, LC45, LC50, LC51, LOEC, LOEL, MATC, MCIG, NOEC, NOEL."
  )
})

test_that("errors bad trophic group", {
  data <- data.frame(
    latin_name = c("a", "b", "c"),
    endpoint = c("NOEL", "EC50", "LC34"),
    effect = c("Mortality", "Reproduction", "Mortality"),
    lifestage = c("Adult", "Young adult", "Embryo"),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    effect_conc_std_mg.L = c(1, 2, 3),
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
    lifestage = c("Adult", "Young adult", "Embryo"),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    effect_conc_std_mg.L = c(1, 2, 3),
    trophic_group = c("Invertebrate", "Plant", "Plant"),
    ecological_group = c("XXXX", "Other", "Other"),
    species_present_in_bc = c("TRUE")
  )
  expect_error(
    wqb_check_add_data(data, template),
    regexp = "The ecological_group column has invalid value\\(s\\). The allowed values include: Planktonic Invertebrate, Other, Salmonid."
  )
})

test_that("errors bad combo of trophic and ecological group", {
  data <- data.frame(
    latin_name = c("a", "b", "c"),
    endpoint = c("NOEL", "EC50", "LC34"),
    effect = c("Mortality", "Reproduction", "Mortality"),
    lifestage = c("Adult", "Young adult", "Embryo"),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    effect_conc_std_mg.L = c(1, 2, 3),
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
    lifestage = c("Adult", "Young adult", "Embryo"),
    effect_conc_mg.L = c(-1.1, 1.2, 1.3),
    effect_conc_std_mg.L = c(1, 2, 3),
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
    lifestage = c("Adult", "Young adult", "Embryo"),
    effect_conc_mg.L = c(10000000, 1.2, 1.3),
    effect_conc_std_mg.L = c(1, 2, 3),
    trophic_group = c("Invertebrate", "Plant", "Plant"),
    ecological_group = c("Other", "Other", "Other"),
    species_present_in_bc = c("TRUE")
  )
  expect_error(
    wqb_check_add_data(data, template),
    regexp = "data\\$effect_conc_mg.L` must have values between 0 and 9e\\+06."
  )
})

test_that("errors bad range in the effect_conc_std_mg.L", {
  data <- data.frame(
    latin_name = c("a", "b", "c"),
    endpoint = c("NOEL", "EC50", "LC34"),
    effect = c("Mortality", "Reproduction", "Mortality"),
    lifestage = c("Adult", "Young adult", "Embryo"),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    effect_conc_std_mg.L = c(-0.1, 2, 3),
    trophic_group = c("Invertebrate", "Plant", "Plant"),
    ecological_group = c("Other", "Other", "Other"),
    species_present_in_bc = c("TRUE")
  )
  expect_error(
    wqb_check_add_data(data, template),
    regexp = "data\\$effect_conc_std_mg.L` must have values between 0 and 9e\\+05."
  )

  data <- data.frame(
    latin_name = c("a", "b", "c"),
    endpoint = c("NOEL", "EC50", "LC34"),
    effect = c("Mortality", "Reproduction", "Mortality"),
    lifestage = c("Adult", "Young adult", "Embryo"),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    effect_conc_std_mg.L = c(10000000, 2, 3),
    trophic_group = c("Invertebrate", "Plant", "Plant"),
    ecological_group = c("Other", "Other", "Other"),
    species_present_in_bc = c("TRUE")
  )
  expect_error(
    wqb_check_add_data(data, template),
    regexp = "data\\$effect_conc_std_mg.L` must have values between 0 and 9e\\+05."
  )
})

test_that("errors bad species_present_in_bc", {
  data <- data.frame(
    latin_name = c("a", "b", "c"),
    endpoint = c("NOEL", "EC50", "LC34"),
    effect = c("Mortality", "Reproduction", "Mortality"),
    lifestage = c("Adult", "Young adult", "Embryo"),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    effect_conc_std_mg.L = c(1, 2, 3),
    trophic_group = c("Invertebrate", "Plant", "Plant"),
    ecological_group = c("Other", "Other", "Other"),
    species_present_in_bc = c("TRUE", "yes", "FALSE")
  )
  expect_error(
    wqb_check_add_data(data, template),
    regexp = "The species_present_in_bc column has invalid value\\(s\\). The allowed values include: TRUE or FALSE."
  )
})

test_that("errors when missing values supplied", {
  data <- data.frame(
    latin_name = c("a", "b", NA_character_),
    endpoint = c("NOEL", "EC50", "LC34"),
    effect = c("Mortality", "Reproduction", "Mortality"),
    lifestage = c("Adult", "Young adult", "Embryo"),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    effect_conc_std_mg.L = c(1, 2, 3),
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
    lifestage = c("Adult", "Young adult", "Embryo"),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    effect_conc_std_mg.L = c(1, 2, 3),
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
    lifestage = c("Adult", "Young adult", "Embryo"),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    effect_conc_std_mg.L = c(1, 2, 3),
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
    lifestage = c("Adult", "Young adult", NA_character_),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    effect_conc_std_mg.L = c(1, 2, 3),
    trophic_group = c("Invertebrate", "Plant", "Plant"),
    ecological_group = c("Other", "Other", "Other"),
    species_present_in_bc = c("TRUE", "TRUE", "FALSE")
  )
  expect_error(
    wqb_check_add_data(data, template),
    regexp = "`data\\$lifestage` must not have any missing values."
  )
  
  data <- data.frame(
    latin_name = c("a", "b", "c"),
    endpoint = c("NOEL", "EC50", "LC50"),
    effect = c("Mortality", "Reproduction", "Mortality"),
    lifestage = c("Adult", "Young adult", "Embryo"),
    effect_conc_mg.L = c(1.1, 1.2, NA_real_),
    effect_conc_std_mg.L = c(1, 2, 3),
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
    lifestage = c("Adult", "Young adult", "Embryo"),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    effect_conc_std_mg.L = c(1, 2, NA_real_),
    trophic_group = c("Invertebrate", "Plant", "Plant"),
    ecological_group = c("Other", "Other", "Other"),
    species_present_in_bc = c("TRUE", "TRUE", "FALSE")
  )
  expect_error(
    wqb_check_add_data(data, template),
    regexp = "`data\\$effect_conc_std_mg.L` must not have any missing values."
  )

  data <- data.frame(
    latin_name = c("a", "b", "c"),
    endpoint = c("NOEL", "EC50", "LC50"),
    effect = c("Mortality", "Reproduction", "Mortality"),
    lifestage = c("Adult", "Young adult", "Embryo"),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    effect_conc_std_mg.L = c(1, 2, 3),
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
    lifestage = c("Adult", "Young adult", "Embryo"),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    effect_conc_std_mg.L = c(1, 2, 3),
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
    lifestage = c("Adult", "Young adult", "Embryo"),
    effect_conc_mg.L = c(1.1, 1.2, 1.3),
    effect_conc_std_mg.L = c(1, 2, 3),
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
    lifestage = c("Adult"),
    effect_conc_std_mg.L = c(1.1),
    trophic_group = c("Plant"),
    ecological_group = c("Other"),
    species_present_in_bc = c("TRUE")
  )

  expect_error(
    wqb_check_add_data(data),
    regexp = 'argument "template" is missing, with no default'
  )
})
