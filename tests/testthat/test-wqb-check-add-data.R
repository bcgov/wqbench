test_that("data passes with single row of data", {
  data <- data.frame(
    latin_name = c("a"),
    endpoint = c("NOEL"),
    effect = c("Mortality"),
    lifestage = c("Adult"),
    effect_conc_std_mg.L = c(1.1),
    trophic_group = c("Plant"),
    ecological_group = c("Other"),
    species_present_in_bc = c(TRUE)
  )
  output <- wqb_check_add_data(data, template)
  expect_equal(output, data)
})

test_that("data passes with multiple rows of data", {
  data <- data.frame(
    latin_name = c("a", "b", "c"),
    endpoint = c("NOEL", "EC50", "LC50"),
    effect = c("Mortality", "Reproduction", "Mortality"),
    lifestage = c("Adult", "Young adult", "Embryo"),
    effect_conc_std_mg.L = c(1, 2, 3),
    trophic_group = c("Fish", "Plant", "Plant"),
    ecological_group = c("Salmonid", "Other", "Other"),
    species_present_in_bc = c(TRUE)
  )
  output <- wqb_check_add_data(data, template)
  expect_equal(output, data)
})

test_that("errors bad endpoint", {
  data <- data.frame(
    latin_name = c("a", "b", "c"),
    endpoint = c("NOEL", "EC50", "X2"),
    effect = c("Mortality", "Reproduction", "Mortality"),
    lifestage = c("Adult", "Young adult", "Embryo"),
    effect_conc_std_mg.L = c(1, 2, 3),
    trophic_group = c("Fish", "Plant", "Plant"),
    ecological_group = c("Salmonid", "Other", "Other"),
    species_present_in_bc = c(TRUE)
  )
  
  expect_error(
    wqb_check_add_data(data, template),
    regexp = "The endpoint column has invalid value\\(s\\)\\. The allowed values include\\: \\(log\\)EC50, \\(log\\)LC50, EC05, EC08, EC10, EC12.5, EC13, EC15, EC16, EC18, EC20, EC22, EC23, EC24, EC25, EC30, EC31, EC32, EC34, EC35, EC37, EC38, EC40, EC41, EC45, EC46, EC50, EC52, EC55, EC58, IC05, IC07, IC10, IC15, IC16, IC20, IC25, IC27, IC30, IC40, IC50, LC05, LC08, LC10, LC15, LC16, LC20, LC25, LC30, LC31, LC34, LC35, LC38, LC40, LC45, LC50, LC51, LOEC, LOEL, MATC, MCIG, NOEC, NOEL."
  )
})
