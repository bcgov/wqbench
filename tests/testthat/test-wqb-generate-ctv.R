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

test_that("method column can't have missing values", {
  reps <- 4L
  df <- data.frame(
    "sp_aggre_conc_mg.L" = rep(1, reps),
    "method" = rep(NA_character_, reps),
    "species_number" = rep(NA, reps),
    "trophic_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps)
  )
  expect_error(
    wqb_generate_ctv(df),
    "must not have any missing values"
  )
})

test_that("conc column can't have missing values", {
  reps <- 4L
  df <- data.frame(
    "sp_aggre_conc_mg.L" = rep(NA_real_, reps),
    "method" = rep("SSD", reps),
    "species_number" = rep(NA, reps),
    "trophic_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps)
  )
  expect_error(
    wqb_generate_ctv(df),
    "must not have any missing values"
  )
})

test_that("det method is used when method is deterministic", {
  reps <- 6L
  df <- data.frame(
    "sp_aggre_conc_mg.L" = c(1, 2, 1.5, 3, 4, 2.5),
    "method" = rep("Deterministic", reps),
    "species_number" = rep(NA, reps),
    "trophic_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps)
  )
  output <- wqb_generate_ctv(df, dists = c("lnorm", "llogis"))
  expect_equal(
    output$ctv_est_mg.L,
    1
  )
  expect_equal(
    output$ctv_lcl_mg.L,
    NA_real_
  )
  expect_equal(
    output$ctv_ucl_mg.L,
    NA_real_
  )
})

test_that("ssd method is used when method is ssd", {
  skip_if_testing_quick()
  
  set.seed(10)
  reps <- 6L
  df <- data.frame(
    "sp_aggre_conc_mg.L" = c(1, 2, 1.5, 3, 4, 2.5),
    "method" = rep("SSD", reps),
    "species_number" = rep(NA, reps),
    "trophic_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps)
  )
  output <- wqb_generate_ctv(df, dists = c("lnorm", "llogis"))
  expect_equal(
    signif(output$ctv_est_mg.L, 3),
    0.99
  )
  expect_equal(
    signif(output$ctv_lcl_mg.L, 3),
    0.537
  )
  expect_equal(
    signif(output$ctv_ucl_mg.L, 3),
    1.86
  )
})

test_that("check ssdtools functions directly", {
  skip_if_testing_quick()
  
  set.seed(10)
  reps <- 15L
  
  df <- data.frame(
    "sp_aggre_conc_mg.L" = rep(c(1, 2, 1.5, 3, 4), 3),
    "method" = rep("SSD", reps),
    "species_number" = rep(NA, reps),
    "trophic_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps)
  )
  output <- ssdtools::ssd_fit_bcanz(
    data = df,
    left = "sp_aggre_conc_mg.L"
  ) |>
  ssdtools::ssd_hc_bcanz(nboot = 10)
  
  expect_snapshot_data(output, "ssdtool_bcanz")
})

test_that("check ssdtools functions directly", {
  skip_if_testing_quick()
  
  set.seed(10)
  reps <- 6L
  df <- data.frame(
    "sp_aggre_conc_mg.L" = c(1, 2, 1.5, 3, 4, 2.5),
    "method" = rep("SSD", reps),
    "species_number" = rep(NA, reps),
    "trophic_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps)
  )
  output <- suppressWarnings(
    ssdtools::ssd_fit_bcanz(
      data = df,
      left = "sp_aggre_conc_mg.L"
    ) |>
    ssdtools::ssd_hc(
      nboot = 100, ci = TRUE, average = FALSE, delta = 10, min_pboot = 0.9
    )
  )
  
  expect_snapshot_data(output, "ssdtool_hc")
})


test_that("check ssdtools fit bcanz and hc functions directly", {
  skip_if_testing_quick()
  
  set.seed(10)
  reps <- 6L
  df <- data.frame(
    "sp_aggre_conc_mg.L" = c(1, 2, 1.5, 3, 4, 2.5),
    "method" = rep("SSD", reps),
    "species_number" = rep(NA, reps),
    "trophic_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps)
  )
  output <- suppressWarnings(
    ssdtools::ssd_fit_bcanz(
      data = df,
      left = "sp_aggre_conc_mg.L"
    ) |>
    ssdtools::ssd_hc(
      nboot = 100, ci = TRUE, average = FALSE, delta = 10, min_pboot = 0.9
    )
  )
  
  expect_equal(
    output$dist,
    c("gamma", "lgumbel", "llogis", "lnorm", "weibull")
  )
  
  expect_equal(
    signif(output$est, 3),
    c(0.951, 1.030, 0.972, 1.000, 0.834)
  )
  
  expect_equal(
    signif(output$se, 3),
    c(0.336, 0.239, 0.342, 0.296, 0.403)
  )
  
  expect_equal(
    signif(output$lcl, 3),
    c(0.623, 0.707, 0.479, 0.68, 0.354)
  )
  
  expect_equal(
    signif(output$ucl, 3),
    c(1.91, 1.75, 1.85, 1.88, 1.75)
  )
  
  expect_equal(
    signif(output$pboot, 3),
    c(1, 1, 1, 1, 1)
  )
})

test_that("check ssdtools fit bcanz and bcanz hc functions", {
  skip_if_testing_quick()
  
  set.seed(10)
  reps <- 6L
  df <- data.frame(
    "sp_aggre_conc_mg.L" = c(1, 2, 1.5, 3, 4, 2.5),
    "method" = rep("SSD", reps),
    "species_number" = rep(NA, reps),
    "trophic_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps)
  )
  
  output <- suppressWarnings(
    ssdtools::ssd_fit_bcanz(
      data = df,
      left = "sp_aggre_conc_mg.L"
    ) |>
    ssdtools::ssd_hc_bcanz(nboot = 100)
  )
  
  expect_equal(
    signif(output$est, 3),
    c(0.615, 0.959, 1.160, 1.450)
  )
  
  expect_equal(
    signif(output$se, 3),
    c(0.315, 0.329, 0.342, 0.368)
  )
  
  expect_equal(
    signif(output$lcl, 3),
    c(0.225, 0.473, 0.666, 0.983)
  )
  
  expect_equal(
    signif(output$ucl, 3),
    c(1.43, 1.77, 2.02, 2.35)
  )
  
  expect_equal(
    signif(output$pboot, 3),
    c(1, 1, 1, 1)
  )
})
