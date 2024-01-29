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
  output <- wqb_generate_ctv(df)
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
  output <- wqb_generate_ctv(df)
  expect_equal(
    signif(output$ctv_est_mg.L, 3),
    0.959
  )
  expect_equal(
    signif(output$ctv_lcl_mg.L, 3),
    0.469
  )
  expect_equal(
    signif(output$ctv_ucl_mg.L, 3),
    1.87
  )
})

# test_that("check ssdtools functions directly", {
#   skip_if_testing_quick()
#   
#   set.seed(10)
#   reps <- 6L
#   df <- data.frame(
#     "sp_aggre_conc_mg.L" = c(1, 2, 1.5, 3, 4, 2.5),
#     "method" = rep("SSD", reps),
#     "species_number" = rep(NA, reps),
#     "trophic_group" = factor(rep(NA, reps)),
#     "species_present_in_bc" = rep(NA, reps),
#     "ecological_group" = factor(rep(NA, reps)),
#     "chemical_name" = rep(NA, reps),
#     "cas" = rep(NA, reps),
#     "latin_name" = rep(NA, reps),
#     "common_name" = rep(NA, reps),
#     "effect" = rep(NA, reps)
#   )
#   output <- ssdtools::ssd_fit_bcanz(
#     data = df,
#     left = "sp_aggre_conc_mg.L"
#   ) |>
#     ssdtools::ssd_hc_bcanz(nboot = 1000) |>
#     dplyr::filter(.data$proportion == 0.05)
#   
#   expect_snapshot_data(output, "ssdtool_bcanz")
# })

test_that("check ssdtools functions directly raw", {
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
  output <- ssdtools::ssd_fit_bcanz(
    data = df,
    left = "sp_aggre_conc_mg.L"
  ) |>
    ssdtools::ssd_hc(
      nboot = 1000, ci = TRUE, average = FALSE, delta = 10, min_pboot = 0.9
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
    c(0.380, 0.266, 0.344, 0.310, 0.397)
  )
  
  expect_equal(
    signif(output$lcl, 3),
    c(0.480, 0.729, 0.524, 0.654, 0.353)
  )
  
  expect_equal(
    signif(output$ucl, 3),
    c(1.99, 1.74, 1.85, 1.92, 1.87)
  )
  
  expect_equal(
    signif(output$pboot, 3),
    c(1, 1, 1, 1, 1)
  )
})

test_that("check ssdtools functions directly 3", {
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
  output <- ssdtools::ssd_fit_bcanz(
    data = df,
    left = "sp_aggre_conc_mg.L"
  ) |>
    ssdtools::ssd_hc_bcanz(nboot = 1000)
  
  expect_equal(
    signif(output$est, 3),
    c(0.615, 0.959, 1.160, 1.450)
  )
  
  expect_equal(
    signif(output$se, 3),
    c(0.341, 0.362, 0.365, 0.368)
  )
  
  expect_equal(
    signif(output$lcl, 3),
    c(0.198, 0.469, 0.647, 0.908)
  )
  
  expect_equal(
    signif(output$ucl, 3),
    c(1.52, 1.87, 2.08, 2.35)
  )
  
  expect_equal(
    signif(output$pboot, 3),
    c(0.999, 0.999, 0.999, 0.999)
  )
  
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
  output <- ssdtools::ssd_fit_bcanz(
    data = df,
    left = "sp_aggre_conc_mg.L"
  ) |>
    ssdtools::ssd_hc(
      nboot = 1000, ci = TRUE, average = FALSE, delta = 10, min_pboot = 0.9
    )
  
  expect_snapshot_data(output, "ssdtool_hc")
})
