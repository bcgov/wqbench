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

test_that("type matches ggplot", {
  skip_if_testing_quick()
  
  reps <- 6L
  df <- data.frame(
    "sp_aggre_conc_mg.L" = c(1, 2, 3, 4, 5, 1),
    "species_number" = rep(NA, reps),
    "trophic_group" = factor(rep(NA, reps)),
    "species_present_in_bc" = rep(NA, reps),
    "ecological_group" = factor(rep(NA, reps)),
    "chemical_name" = rep(NA, reps),
    "cas" = rep(NA, reps),
    "latin_name" = rep(NA, reps),
    "common_name" = rep(NA, reps),
    "effect" = rep(NA, reps),
    "method" = rep(NA_character_, reps)
  )
  fit <- wqb_ssd_fit(df)
  output <- wqb_plot_ssd(df, fit)
  expect_equal(class(output), c("gg", "ggplot"))
})
