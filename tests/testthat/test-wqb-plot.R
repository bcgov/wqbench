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

test_that("erros when latin name missing ", {
  reps <- 1
  df <- data.frame(
    "species_number" = rep(NA, reps),
    "lifestage" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "effect_conc_mg.L" = rep(NA, reps),
    "endpoint" = rep(NA, reps),
    "method" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA_character_, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_std_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps),
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),
    "version" = rep(NA, reps)
  )
  expect_error(
    wqb_plot(df),
    "`data\\$latin_name` must not have any missing values"
  )
})

test_that("erros when effect conc is missing ", {
  reps <- 1
  df <- data.frame(
    "latin_name" = rep("Blue Heron", reps),
    "effect_conc_mg.L" = rep(NA_real_, reps),
    "endpoint" = rep(NA, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "species_number" = rep(NA, reps),
    "lifestage" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "method" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_std_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps),
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),
    "version" = rep(NA, reps)
  )
  expect_error(
    wqb_plot(df),
    "`data\\$effect_conc_mg.L` must not have any missing values"
  )
})

test_that("erros when endpoint is missing ", {
  reps <- 1
  df <- data.frame(
    "latin_name" = rep("Blue Heron", reps),
    "effect_conc_mg.L" = rep(1, reps),
    "endpoint" = rep(NA_character_, reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "species_number" = rep(NA, reps),
    "lifestage" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "method" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_std_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps),
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),
    "version" = rep(NA, reps)
  )
  expect_error(
    wqb_plot(df),
    "`data\\$endpoint` must not have any missing values"
  )
})

test_that("erros when trophic group is missing ", {
  reps <- 1
  df <- data.frame(
    "latin_name" = rep("Blue Heron", reps),
    "effect_conc_mg.L" = rep(1, reps),
    "endpoint" = rep("NOEC", reps),
    "trophic_group" = factor(rep(NA_character_, reps)),
    "species_number" = rep(NA, reps),
    "lifestage" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "method" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_std_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps),
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),
    "version" = rep(NA, reps)
  )
  expect_error(
    wqb_plot(df),
    "`data\\$trophic_group` must not have any missing values"
  )
})

test_that("plot type is ggplot", {
  reps <- 1
  df <- data.frame(
    "latin_name" = rep("Blue Heron", reps),
    "effect_conc_mg.L" = rep(1, reps),
    "endpoint" = rep("NOEC", reps),
    "trophic_group" = factor(rep("Fish", reps)),
    "species_number" = rep(NA, reps),
    "lifestage" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "method" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_std_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps),
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),
    "version" = rep(NA, reps)
  )
  output <- wqb_plot(df)
  expect_equal(class(output), c("gg", "ggplot"))
  expect_snapshot_plot(output, "wqb_plot")
})

test_that("shape is present when there are 5 groups or less", {
  reps <- 5
  df <- data.frame(
    "latin_name" = rep("Blue Heron", reps),
    "effect_conc_mg.L" = c(1, 2, 3, 4, 5),
    "endpoint" = c("NOEC", "LOEC", "EC08", "EC10", "EC12.5"),
    "trophic_group" = factor(rep("Fish", reps)),
    "species_number" = rep(NA, reps),
    "lifestage" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "method" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_std_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps),
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),
    "version" = rep(NA, reps)
  )
  output <- wqb_plot(df)
  expect_equal(class(output), c("gg", "ggplot"))
  expect_snapshot_plot(output, "shape_5_groups")
})

test_that("shape is removed after 6 endpoint group are present set", {
  reps <- 6
  df <- data.frame(
    "latin_name" = rep("Blue Heron", reps),
    "effect_conc_mg.L" = c(1, 2, 3, 4, 5, 6),
    "endpoint" = c("NOEC", "LOEC", "EC08", "EC10", "EC12.5", "EC13"),
    "trophic_group" = factor(rep("Fish", reps)),
    "species_number" = rep(NA, reps),
    "lifestage" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "method" = rep(NA, reps),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect_conc_std_mg.L" = rep(NA, reps),
    "acr" = rep(NA, reps),
    "media_type" = rep(NA, reps),
    "ecological_group" = rep(NA, reps),
    "species_present_in_bc" = rep(NA, reps),
    "author" = rep(NA, reps),
    "title" = rep(NA, reps),
    "source" = rep(NA, reps),
    "publication_year" = rep(NA, reps),
    "download_date" = rep(NA, reps),
    "version" = rep(NA, reps)
  )
  output <- wqb_plot(df)
  expect_equal(class(output), c("gg", "ggplot"))
  expect_snapshot_plot(output, "color_6_groups")
})
