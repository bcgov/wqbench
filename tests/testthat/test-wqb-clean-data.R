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

test_that("function outputs 1 row when given 1 row and no message when set to quiet", {
  df <- create_clean_test_data()  
  output <- wqbench:::wqb_clean_data(df, quiet = TRUE) 
  expect_equal(
    nrow(output),
    1L
  )
})

test_that("message is output when quiet is false", {
  df <- create_clean_test_data()  
  expect_message(
    wqbench:::wqb_clean_data(df, quiet = FALSE) ,
    regexp = "Clean data"
  )
}) 

test_that("row with no conc1 value is removed", {
  df <- create_clean_test_data(
    list(conc1_mean = "NR")
  )  
  output <- wqbench:::wqb_clean_data(df, quiet = TRUE)
  expect_equal(
    nrow(output),
    0L
  )
}) 

test_that("row with < conc1 value is removed", {
  df <- create_clean_test_data(
    list(conc1_mean = "<1000")
  )  
  output <- wqbench:::wqb_clean_data(df, quiet = TRUE)
  expect_equal(
    nrow(output),
    0L
  )
}) 

test_that("row with no genus value is removed", {
  df <- create_clean_test_data(
    list(genus = NA_character_)
  )  
  output <- wqbench:::wqb_clean_data(df, quiet = TRUE)
  expect_equal(
    nrow(output),
    0L
  )
}) 

test_that("row with no trophic group value is removed", {
  df <- create_clean_test_data(
    list(trophic_group = NA_character_)
  )  
  output <- wqbench:::wqb_clean_data(df, quiet = TRUE)
  expect_equal(
    nrow(output),
    0L
  )
}) 

test_that("row with blank duration mean value is removed", {
  df <- create_clean_test_data(
    list(duration_mean = "")
  )  
  output <- wqbench:::wqb_clean_data(df, quiet = TRUE)
  expect_equal(
    nrow(output),
    0L
  )
}) 

test_that("row with NR duration mean value is removed", {
  df <- create_clean_test_data(
    list(duration_mean = "NR")
  )  
  output <- wqbench:::wqb_clean_data(df, quiet = TRUE)
  expect_equal(
    nrow(output),
    0L
  )
}) 

test_that("row with NA duration mean value is removed", {
  df <- create_clean_test_data(
    list(duration_mean = NA_real_)
  )  
  output <- wqbench:::wqb_clean_data(df, quiet = TRUE)
  expect_equal(
    nrow(output),
    0L
  )
}) 

test_that("row with FALSE duration_units_to_keep value is removed", {
  df <- create_clean_test_data(
    list(duration_units_to_keep = FALSE)
  )  
  output <- wqbench:::wqb_clean_data(df, quiet = TRUE)
  expect_equal(
    nrow(output),
    0L
  )
}) 

test_that("row with FALSE conc_conversion_flag value is removed", {
  df <- create_clean_test_data(
    list(conc_conversion_flag = FALSE)
  )  
  output <- wqbench:::wqb_clean_data(df, quiet = TRUE)
  expect_equal(
    nrow(output),
    0L
  )
}) 

test_that("endpoint has asterick removed", {
  df <- create_clean_test_data(
    list(endpoint = "LC50*")
  )  
  output <- wqbench:::wqb_clean_data(df, quiet = TRUE)
  expect_equal(
    output$endpoint,
    "LC50"
  )
}) 

test_that("conc1_mean has asterick removed and is output as a real number", {
  df <- create_clean_test_data(
    list(conc1_mean = "100*")
  )  
  output <- wqbench:::wqb_clean_data(df, quiet = TRUE)
  expect_equal(
    output$conc1_mean,
    100
  )
  expect_equal(
    typeof(output$conc1_mean),
    "double"
  )
}) 

test_that("duration mean converted by multiplier when already hours", {
  df <- create_clean_test_data()  
  output <- wqbench:::wqb_clean_data(df, quiet = TRUE)
  expect_equal(
    output$duration_mean,
    1
  )
  expect_equal(
    output$duration_value_multiplier_to_hours,
    1
  )
  expect_equal(
    output$duration_units_to_keep,
    TRUE
  )
  expect_equal(
    output$duration_hrs,
    1
  )
}) 

test_that("duration mean converted by multiplier is 24", {
  df <- create_clean_test_data(
    list(duration_value_multiplier_to_hours = 24)
  )  
  output <- wqbench:::wqb_clean_data(df, quiet = TRUE)
  expect_equal(
    output$duration_mean,
    1
  )
  expect_equal(
    output$duration_value_multiplier_to_hours,
    24
  )
  expect_equal(
    output$duration_units_to_keep,
    TRUE
  )
  expect_equal(
    output$duration_hrs,
    24
  )
}) 

test_that("duration mean converted by multiplier is 24 and mean is 2", {
  df <- create_clean_test_data(
    list(
      duration_mean = 2,
      duration_value_multiplier_to_hours = 24
    )
  )  
  output <- wqbench:::wqb_clean_data(df, quiet = TRUE)
  expect_equal(
    output$duration_mean,
    2
  )
  expect_equal(
    output$duration_value_multiplier_to_hours,
    24
  )
  expect_equal(
    output$duration_units_to_keep,
    TRUE
  )
  expect_equal(
    output$duration_hrs,
    48
  )
}) 

test_that("conc1 mean converted by multiplier", {
  df <- create_clean_test_data()  
  output <- wqbench:::wqb_clean_data(df, quiet = TRUE)
  expect_equal(
    output$conc1_mean,
    1
  )
  expect_equal(
    output$conc_conversion_value_multiplier,
    1
  )
  expect_equal(
    output$conc_conversion_flag,
    TRUE
  )
  expect_equal(
    output$effect_conc_mg.L,
    1
  )
}) 

test_that("conc1 mean converted by multiplier when multipler is 2", {
  df <- create_clean_test_data(
    list(
      conc_conversion_value_multiplier = 2
    )
  )  
  output <- wqbench:::wqb_clean_data(df, quiet = TRUE)
  expect_equal(
    output$conc1_mean,
    1
  )
  expect_equal(
    output$conc_conversion_value_multiplier,
    2
  )
  expect_equal(
    output$conc_conversion_flag,
    TRUE
  )
  expect_equal(
    output$effect_conc_mg.L,
    2
  )
}) 

test_that("conc1 mean converted by multiplier when multipler is 2 and conc1 value is 0.01", {
  df <- create_clean_test_data(
    list(
      conc1_mean = 0.01,
      conc_conversion_value_multiplier = 2
    )
  )  
  output <- wqbench:::wqb_clean_data(df, quiet = TRUE)
  expect_equal(
    output$conc1_mean,
    0.01
  )
  expect_equal(
    output$conc_conversion_value_multiplier,
    2
  )
  expect_equal(
    output$conc_conversion_flag,
    TRUE
  )
  expect_equal(
    output$effect_conc_mg.L,
    0.02
  )
}) 

test_that("conc1 mean converted by multiplier when multipler is 0.0001 and conc1 value is 10", {
  df <- create_clean_test_data(
    list(
      conc1_mean = 10,
      conc_conversion_value_multiplier = 0.0001
    )
  )  
  output <- wqbench:::wqb_clean_data(df, quiet = TRUE)
  expect_equal(
    output$conc1_mean,
    10
  )
  expect_equal(
    output$conc_conversion_value_multiplier,
    0.0001
  )
  expect_equal(
    output$conc_conversion_flag,
    TRUE
  )
  expect_equal(
    output$effect_conc_mg.L,
    0.001
  )
}) 

test_that("adult coded for missing lifestage for fish", {
  df <- create_clean_test_data(
    list(
      trophic_group = "Fish",
      lifestage_description = NA_character_,
      simple_lifestage = NA_character_
    )
  )  
  output <- wqbench:::wqb_clean_data(df, quiet = TRUE)
  expect_equal(
    output$simple_lifestage,
    "adult"
  )
}) 

test_that("adult coded for missing lifestage for amphibian", {
  df <- create_clean_test_data(
    list(
      trophic_group = "Amphibian",
      lifestage_description = NA_character_,
      simple_lifestage = NA_character_
    )
  )  
  output <- wqbench:::wqb_clean_data(df, quiet = TRUE)
  expect_equal(
    output$simple_lifestage,
    "adult"
  )
}) 

test_that("NA coded for simple lifestage for plant", {
  df <- create_clean_test_data(
    list(
      trophic_group = "Plant",
      lifestage_description = NA_character_,
      simple_lifestage = NA_character_
    )
  )  
  output <- wqbench:::wqb_clean_data(df, quiet = TRUE)
  expect_equal(
    output$simple_lifestage,
    NA_character_
  )
})

test_that("NA coded for simple lifestage for plant when lifestage is adult", {
  df <- create_clean_test_data(
    list(
      trophic_group = "Plant",
      lifestage_description = 'adult',
      simple_lifestage = NA_character_
    )
  )  
  output <- wqbench:::wqb_clean_data(df, quiet = TRUE)
  expect_equal(
    output$simple_lifestage,
    NA_character_
  )
}) 

test_that("NA coded for simple lifestage for algae", {
  df <- create_clean_test_data(
    list(
      trophic_group = "Algae",
      lifestage_description = NA_character_,
      simple_lifestage = NA_character_
    )
  )  
  output <- wqbench:::wqb_clean_data(df, quiet = TRUE)
  expect_equal(
    output$simple_lifestage,
    NA_character_
  )
})

test_that("NA coded for simple lifestage for algae when lifestage is adult", {
  df <- create_clean_test_data(
    list(
      trophic_group = "Algae",
      lifestage_description = "Adult",
      simple_lifestage = NA_character_
    )
  )  
  output <- wqbench:::wqb_clean_data(df, quiet = TRUE)
  expect_equal(
    output$simple_lifestage,
    NA_character_
  )
})

test_that("NA coded for simple lifestage for invertebrate", {
  df <- create_clean_test_data(
    list(
      trophic_group = "Invertebrate",
      lifestage_description = NA_character_,
      simple_lifestage = NA_character_
    )
  )  
  output <- wqbench:::wqb_clean_data(df, quiet = TRUE)
  expect_equal(
    output$simple_lifestage,
    NA_character_
  )
})

test_that("NA coded for simple lifestage for invertebrate when lifestage is adult", {
  df <- create_clean_test_data(
    list(
      trophic_group = "Invertebrate",
      lifestage_description = "Adult",
      simple_lifestage = NA_character_
    )
  )  
  output <- wqbench:::wqb_clean_data(df, quiet = TRUE)
  expect_equal(
    output$simple_lifestage,
    NA_character_
  )
})

test_that("NA coded for simple lifestage for invertebrate", {
  df <- create_clean_test_data(
    list(
      trophic_group = "Invertebrate",
      lifestage_description = NA_character_,
      simple_lifestage = NA_character_
    )
  )  
  output <- wqbench:::wqb_clean_data(df, quiet = TRUE)
  expect_equal(
    output$simple_lifestage,
    NA_character_
  )
})

test_that("cas number is a character", {
  df <- create_clean_test_data(
    list(
      test_cas = 123L
    )
  )  
  output <- wqbench:::wqb_clean_data(df, quiet = TRUE)
  expect_equal(
    output$cas,
    "123"
  )
  expect_equal(
    typeof(output$cas),
    "character"
  )
})

test_that("ecological group is a factor", {
  df <- create_clean_test_data()  
  output <- wqbench:::wqb_clean_data(df, quiet = TRUE)
  expect_equal(
    class(output$ecological_group),
    "factor"
  )
})

test_that("trophic group is a factor", {
  df <- create_clean_test_data()  
  output <- wqbench:::wqb_clean_data(df, quiet = TRUE)
  expect_equal(
    class(output$trophic_group),
    "factor"
  )
})

test_that("test with sample data", {
  raw_data <- wqbenchdata::get_wqbenchdata("raw_data")
  output <- wqbench:::wqb_clean_data(raw_data, quiet = TRUE) |>
    dplyr::arrange(chemical_name)
  expect_equal(
    nrow(output),
    18L
  )
  expect_equal(
    signif(output$effect_conc_mg.L, 4),
    signif(c(
      1.6650,
      0.015,
      0.0404,
      0.179,
      30.4,
      1,
      10,
      0.1000,
      0.392,
      2,
      0.507,
      0.56,
      99.4,
      1.75,
      0.15,
      2,
      18.3,
      0.93
    ), 4)
  )
  expect_equal(
    output$duration_hrs,
    c(20L, 672L, 96L, 1680L, 3L, 24L, 96L, 168L, 72L, 72L, 2304L, 96L, 24L, 96L, 
      240L, 168L, 96L, 336L)
  )
  expect_equal(
    class(output$trophic_group),
    "factor"
  )
  expect_equal(
    class(output$ecological_group),
    "factor"
  )
  expect_equal(
    output$simple_lifestage[output$trophic_group == "Algae"],
    c(NA_character_, NA_character_)
  )
  expect_equal(
    output$simple_lifestage[output$trophic_group == "Invertebrate"],
    c(NA_character_, NA_character_, NA_character_)
  )
  expect_equal(
    output$simple_lifestage[output$trophic_group == "Plant"],
    c(NA_character_)
  )
  expect_true(
    all(!is.na(output$simple_lifestage[output$trophic_group == "Fish"]))
  )
  expect_true(
    all(!is.na(output$simple_lifestage[output$trophic_group == "Amphibian"]))
  )
})

test_that("test sample data selected duration correct from study and observed values", {
  raw_data <- wqbenchdata::get_wqbenchdata("raw_data")
  output <- wqbench:::wqb_clean_data(raw_data, quiet = TRUE) |>
    dplyr::arrange(chemical_name)
  expect_equal(
    output$duration_mean[output$chemical_name == "2,3,4,5,6-Pentachlorophenol"],
    20
  )
  expect_equal(
    output$duration_mean[output$cas == "79622596" & output$effect == "Population"],
    10
  )
  expect_equal(
    output$duration_hrs[output$cas == "79622596" & output$effect == "Population"],
    1680
  )
  expect_equal(
    output$duration_mean[output$cas == "122349"],
    72
  )
  expect_equal(
    output$duration_mean[output$cas == "420042"],
    96
  )
  expect_equal(
    output$duration_hrs[output$cas == "420042"],
    2304
  )
})

test_that("test sample data conc conv executed", {
  raw_data <- wqbenchdata::get_wqbenchdata("raw_data")
  output <- wqbench:::wqb_clean_data(raw_data, quiet = TRUE) |>
    dplyr::arrange(chemical_name)
  expect_equal(
    output$conc1_mean[output$cas == "151213"],
    0.150
  )
  expect_equal(
    output$effect_conc_mg.L[output$cas == "151213"],
    0.150
  )
  expect_equal(
    output$conc1_mean[output$cas == "87865"],
    1665.0
  )
  expect_equal(
    output$effect_conc_mg.L[output$cas == "87865"],
    1.6650
  )
})
