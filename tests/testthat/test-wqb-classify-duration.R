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

# General ----
test_that("check message output", {
  reps <- 1
  df <- data.frame(
    "trophic_group" = factor(rep("fish", reps)),
    "duration_hrs" = c(1), 
    "ecological_group" = factor(rep("other", reps)), 
    "simple_lifestage" = rep("adult", reps),
    "chemical_name" = rep(NA, reps),  
    "cas" = rep(NA, reps),  
    "endpoint" = rep(NA, reps),   
    "effect" = rep(NA, reps),   
    "effect_conc_mg.L" = rep(NA, reps),   
    "organism_habitat" = rep(NA, reps),   
    "species_number" = rep(NA, reps),   
    "latin_name" = rep(NA, reps),   
    "common_name" = rep(NA, reps),   
    "species_present_in_bc" = rep(NA, reps),   
    "lifestage" = rep(NA, reps),   
    "media_type" = rep(NA, reps),   
    "present_in_bc_wqg" = rep(NA, reps),   
    "author" = rep(NA, reps),   
    "title" = rep(NA, reps),   
    "source" = rep(NA, reps),   
    "publication_year" = rep(NA, reps),   
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps)
  ) 
  expect_message(
    wqb_classify_duration(df),
    regexp = "Classify duration"
  )
})

# Fish ----
test_that("fish adult is acute under 504 hours and chronic over", {
  reps <- 11
  df <- data.frame(
    "trophic_group" = factor(rep("fish", reps)),
    "duration_hrs" = c(1, 95, 96, 97, 167, 168, 169, 503, 504, 505, 1000), 
    "ecological_group" = factor(rep("other", reps)), 
    "simple_lifestage" = rep("adult", reps),
    "chemical_name" = rep(NA, reps),  
    "cas" = rep(NA, reps),  
    "endpoint" = rep(NA, reps),   
    "effect" = rep(NA, reps),   
    "effect_conc_mg.L" = rep(NA, reps),   
    "organism_habitat" = rep(NA, reps),   
    "species_number" = rep(NA, reps),   
    "latin_name" = rep(NA, reps),   
    "common_name" = rep(NA, reps),   
    "species_present_in_bc" = rep(NA, reps),   
    "lifestage" = rep(NA, reps),   
    "media_type" = rep(NA, reps),   
    "present_in_bc_wqg" = rep(NA, reps),   
    "author" = rep(NA, reps),   
    "title" = rep(NA, reps),   
    "source" = rep(NA, reps),   
    "publication_year" = rep(NA, reps),   
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps)
  ) 
  output <- wqb_classify_duration(df, quiet = TRUE)
  expect_equal(
    output$duration_class,
#hours   1        95       96       97       167     168      169      503     504         505        1000
    c("acute", "acute", "acute", "acute", "acute", "acute", "acute", "acute", "chronic", "chronic", "chronic")
  )
})

test_that("fish juvenile is acute under 504 hours and chronic over", {
  reps <- 11
  df <- data.frame(
    "trophic_group" = factor(rep("fish", reps)),
    "duration_hrs" = c(1, 95, 96, 97, 167, 168, 169, 503, 504, 505, 1000), 
    "ecological_group" = factor(rep("other", reps)), 
    "simple_lifestage" = rep("juvenile", reps),
    "chemical_name" = rep(NA, reps),  
    "cas" = rep(NA, reps),  
    "endpoint" = rep(NA, reps),   
    "effect" = rep(NA, reps),   
    "effect_conc_mg.L" = rep(NA, reps),   
    "organism_habitat" = rep(NA, reps),   
    "species_number" = rep(NA, reps),   
    "latin_name" = rep(NA, reps),   
    "common_name" = rep(NA, reps),   
    "species_present_in_bc" = rep(NA, reps),   
    "lifestage" = rep(NA, reps),   
    "media_type" = rep(NA, reps),   
    "present_in_bc_wqg" = rep(NA, reps),   
    "author" = rep(NA, reps),   
    "title" = rep(NA, reps),   
    "source" = rep(NA, reps),   
    "publication_year" = rep(NA, reps),   
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps)
  ) 
  output <- wqb_classify_duration(df, quiet = TRUE)
  expect_equal(
    output$duration_class,
#hours   1        95       96       97       167     168      169      503     504         505        1000
    c("acute", "acute", "acute", "acute", "acute", "acute", "acute", "acute", "chronic", "chronic", "chronic")
  )
})

test_that("fish els is acute under 168 hours and chronic over", {
  reps <- 11
  df <- data.frame(
    "trophic_group" = factor(rep("fish", reps)),
    "duration_hrs" = c(1, 95, 96, 97, 167, 168, 169, 503, 504, 505, 1000), 
    "ecological_group" = factor(rep("other", reps)), 
    "simple_lifestage" = rep("els", reps),
    "chemical_name" = rep(NA, reps),  
    "cas" = rep(NA, reps),  
    "endpoint" = rep(NA, reps),   
    "effect" = rep(NA, reps),   
    "effect_conc_mg.L" = rep(NA, reps),   
    "organism_habitat" = rep(NA, reps),   
    "species_number" = rep(NA, reps),   
    "latin_name" = rep(NA, reps),   
    "common_name" = rep(NA, reps),   
    "species_present_in_bc" = rep(NA, reps),   
    "lifestage" = rep(NA, reps),   
    "media_type" = rep(NA, reps),   
    "present_in_bc_wqg" = rep(NA, reps),   
    "author" = rep(NA, reps),   
    "title" = rep(NA, reps),   
    "source" = rep(NA, reps),   
    "publication_year" = rep(NA, reps),   
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps)
  ) 
  output <- wqb_classify_duration(df, quiet = TRUE)
  expect_equal(
    output$duration_class,
#hours   1        95       96       97       167     168         169       503       504         505        1000
    c("acute", "acute", "acute", "acute", "acute", "chronic", "chronic","chronic", "chronic", "chronic", "chronic")
  )
})

test_that("when fish lifestage missing value is classified as acute", {
  reps <- 1
  df <- data.frame(
    "trophic_group" = factor(rep("fish", reps)),
    "duration_hrs" = c(1), 
    "ecological_group" = factor(rep("other", reps)), 
    "simple_lifestage" = rep(NA_character_, reps),
    "chemical_name" = rep(NA, reps),  
    "cas" = rep(NA, reps),  
    "endpoint" = rep(NA, reps),   
    "effect" = rep(NA, reps),   
    "effect_conc_mg.L" = rep(NA, reps),   
    "organism_habitat" = rep(NA, reps),   
    "species_number" = rep(NA, reps),   
    "latin_name" = rep(NA, reps),   
    "common_name" = rep(NA, reps),   
    "species_present_in_bc" = rep(NA, reps),   
    "lifestage" = rep(NA, reps),   
    "media_type" = rep(NA, reps),   
    "present_in_bc_wqg" = rep(NA, reps),   
    "author" = rep(NA, reps),   
    "title" = rep(NA, reps),   
    "source" = rep(NA, reps),   
    "publication_year" = rep(NA, reps),   
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps)
  ) 
  output <- wqb_classify_duration(df, quiet = TRUE)
  expect_equal(
    output$duration_class,
    c("acute")
  )
})

# Amphibian ----
test_that("amphibian adult is acute under 504 hours and chronic over", {
  reps <- 11
  df <- data.frame(
    "trophic_group" = factor(rep("amphibian", reps)),
    "duration_hrs" = c(1, 95, 96, 97, 167, 168, 169, 503, 504, 505, 1000), 
    "ecological_group" = factor(rep("other", reps)), 
    "simple_lifestage" = rep("adult", reps),
    "chemical_name" = rep(NA, reps),  
    "cas" = rep(NA, reps),  
    "endpoint" = rep(NA, reps),   
    "effect" = rep(NA, reps),   
    "effect_conc_mg.L" = rep(NA, reps),   
    "organism_habitat" = rep(NA, reps),   
    "species_number" = rep(NA, reps),   
    "latin_name" = rep(NA, reps),   
    "common_name" = rep(NA, reps),   
    "species_present_in_bc" = rep(NA, reps),   
    "lifestage" = rep(NA, reps),   
    "media_type" = rep(NA, reps),   
    "present_in_bc_wqg" = rep(NA, reps),   
    "author" = rep(NA, reps),   
    "title" = rep(NA, reps),   
    "source" = rep(NA, reps),   
    "publication_year" = rep(NA, reps),   
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps)
  ) 
  output <- wqb_classify_duration(df, quiet = TRUE)
  expect_equal(
    output$duration_class,
    #hours   1        95       96       97       167     168      169      503     504         505        1000
    c("acute", "acute", "acute", "acute", "acute", "acute", "acute", "acute", "chronic", "chronic", "chronic")
  )
})

test_that("amphibian juvenile is acute under 504 hours and chronic over", {
  reps <- 11
  df <- data.frame(
    "trophic_group" = factor(rep("amphibian", reps)),
    "duration_hrs" = c(1, 95, 96, 97, 167, 168, 169, 503, 504, 505, 1000), 
    "ecological_group" = factor(rep("other", reps)), 
    "simple_lifestage" = rep("juvenile", reps),
    "chemical_name" = rep(NA, reps),  
    "cas" = rep(NA, reps),  
    "endpoint" = rep(NA, reps),   
    "effect" = rep(NA, reps),   
    "effect_conc_mg.L" = rep(NA, reps),   
    "organism_habitat" = rep(NA, reps),   
    "species_number" = rep(NA, reps),   
    "latin_name" = rep(NA, reps),   
    "common_name" = rep(NA, reps),   
    "species_present_in_bc" = rep(NA, reps),   
    "lifestage" = rep(NA, reps),   
    "media_type" = rep(NA, reps),   
    "present_in_bc_wqg" = rep(NA, reps),   
    "author" = rep(NA, reps),   
    "title" = rep(NA, reps),   
    "source" = rep(NA, reps),   
    "publication_year" = rep(NA, reps),   
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps)
  ) 
  output <- wqb_classify_duration(df, quiet = TRUE)
  expect_equal(
    output$duration_class,
    #hours   1        95       96       97       167     168      169      503     504         505        1000
    c("acute", "acute", "acute", "acute", "acute", "acute", "acute", "acute", "chronic", "chronic", "chronic")
  )
})

test_that("amphibian els is acute under 168 hours and chronic over", {
  reps <- 11
  df <- data.frame(
    "trophic_group" = factor(rep("amphibian", reps)),
    "duration_hrs" = c(1, 95, 96, 97, 167, 168, 169, 503, 504, 505, 1000), 
    "ecological_group" = factor(rep("other", reps)), 
    "simple_lifestage" = rep("els", reps),
    "chemical_name" = rep(NA, reps),  
    "cas" = rep(NA, reps),  
    "endpoint" = rep(NA, reps),   
    "effect" = rep(NA, reps),   
    "effect_conc_mg.L" = rep(NA, reps),   
    "organism_habitat" = rep(NA, reps),   
    "species_number" = rep(NA, reps),   
    "latin_name" = rep(NA, reps),   
    "common_name" = rep(NA, reps),   
    "species_present_in_bc" = rep(NA, reps),   
    "lifestage" = rep(NA, reps),   
    "media_type" = rep(NA, reps),   
    "present_in_bc_wqg" = rep(NA, reps),   
    "author" = rep(NA, reps),   
    "title" = rep(NA, reps),   
    "source" = rep(NA, reps),   
    "publication_year" = rep(NA, reps),   
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps)
  ) 
  output <- wqb_classify_duration(df, quiet = TRUE)
  expect_equal(
    output$duration_class,
    #hours   1        95       96       97       167     168         169       503       504         505        1000
    c("acute", "acute", "acute", "acute", "acute", "chronic", "chronic","chronic", "chronic", "chronic", "chronic")
  )
})

test_that("amphibian adult is acute under 504 hours and chronic over", {
  reps <- 11
  df <- data.frame(
    "trophic_group" = factor(rep("amphibian", reps)),
    "duration_hrs" = c(1, 95, 96, 97, 167, 168, 169, 503, 504, 505, 1000), 
    "ecological_group" = factor(rep("other", reps)), 
    "simple_lifestage" = rep("adult", reps),
    "chemical_name" = rep(NA, reps),  
    "cas" = rep(NA, reps),  
    "endpoint" = rep(NA, reps),   
    "effect" = rep(NA, reps),   
    "effect_conc_mg.L" = rep(NA, reps),   
    "organism_habitat" = rep(NA, reps),   
    "species_number" = rep(NA, reps),   
    "latin_name" = rep(NA, reps),   
    "common_name" = rep(NA, reps),   
    "species_present_in_bc" = rep(NA, reps),   
    "lifestage" = rep(NA, reps),   
    "media_type" = rep(NA, reps),   
    "present_in_bc_wqg" = rep(NA, reps),   
    "author" = rep(NA, reps),   
    "title" = rep(NA, reps),   
    "source" = rep(NA, reps),   
    "publication_year" = rep(NA, reps),   
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps)
  ) 
  output <- wqb_classify_duration(df, quiet = TRUE)
  expect_equal(
    output$duration_class,
    #hours   1        95       96       97       167     168      169      503     504         505        1000
    c("acute", "acute", "acute", "acute", "acute", "acute", "acute", "acute", "chronic", "chronic", "chronic")
  )
})

# Invertebrates ----
test_that("invertebrate other is acute under 168 hours and chronic over", {
  reps <- 8
  df <- data.frame(
    "trophic_group" = factor(rep("invertebrate", reps)),
    "duration_hrs" = c(1, 95, 96, 97, 167, 168, 169, 500), 
    "ecological_group" = factor(rep("Other", reps)), 
    "simple_lifestage" = rep(NA_character_, reps),
    "chemical_name" = rep(NA, reps),  
    "cas" = rep(NA, reps),  
    "endpoint" = rep(NA, reps),   
    "effect" = rep(NA, reps),   
    "effect_conc_mg.L" = rep(NA, reps),   
    "organism_habitat" = rep(NA, reps),   
    "species_number" = rep(NA, reps),   
    "latin_name" = rep(NA, reps),   
    "common_name" = rep(NA, reps),   
    "species_present_in_bc" = rep(NA, reps),   
    "lifestage" = rep(NA, reps),   
    "media_type" = rep(NA, reps),   
    "present_in_bc_wqg" = rep(NA, reps),   
    "author" = rep(NA, reps),   
    "title" = rep(NA, reps),   
    "source" = rep(NA, reps),   
    "publication_year" = rep(NA, reps),   
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps)
  ) 
  output <- wqb_classify_duration(df, quiet = TRUE)
  expect_equal(
    output$duration_class,
#hours   1,       95,      96,      97,      167,     168,       169,       500
    c("acute", "acute", "acute", "acute", "acute", "chronic", "chronic", "chronic")
  )
})

test_that("invertebrate plaktonic is acute under 96 hours and chronic over", {
  reps <- 8
  df <- data.frame(
    "trophic_group" = factor(rep("invertebrate", reps)),
    "duration_hrs" = c(1, 95, 96, 97, 167, 168, 169, 500), 
    "ecological_group" = factor(rep("Planktonic Invertebrate", reps)), 
    "simple_lifestage" = rep(NA_character_, reps),
    "chemical_name" = rep(NA, reps),  
    "cas" = rep(NA, reps),  
    "endpoint" = rep(NA, reps),   
    "effect" = rep(NA, reps),   
    "effect_conc_mg.L" = rep(NA, reps),   
    "organism_habitat" = rep(NA, reps),   
    "species_number" = rep(NA, reps),   
    "latin_name" = rep(NA, reps),   
    "common_name" = rep(NA, reps),   
    "species_present_in_bc" = rep(NA, reps),   
    "lifestage" = rep(NA, reps),   
    "media_type" = rep(NA, reps),   
    "present_in_bc_wqg" = rep(NA, reps),   
    "author" = rep(NA, reps),   
    "title" = rep(NA, reps),   
    "source" = rep(NA, reps),   
    "publication_year" = rep(NA, reps),   
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps)
  ) 
  output <- wqb_classify_duration(df, quiet = TRUE)
  expect_equal(
    output$duration_class,
#hours   1,       95,      96,      97,        167,       168,       169,       500
    c("acute", "acute", "acute", "chronic", "chronic", "chronic", "chronic", "chronic")
  )
})

# Algae ----
test_that("algae is acute under 24 hours and chronic over", {
  reps <- 5
  df <- data.frame(
    "trophic_group" = factor(rep("algae", reps)),
    "duration_hrs" = c(1, 23, 24, 25, 50), 
    "ecological_group" = factor(rep("other", reps)), 
    "simple_lifestage" = rep(NA_character_, reps),
    "chemical_name" = rep(NA, reps),  
    "cas" = rep(NA, reps),  
    "endpoint" = rep(NA, reps),   
    "effect" = rep(NA, reps),   
    "effect_conc_mg.L" = rep(NA, reps),   
    "organism_habitat" = rep(NA, reps),   
    "species_number" = rep(NA, reps),   
    "latin_name" = rep(NA, reps),   
    "common_name" = rep(NA, reps),   
    "species_present_in_bc" = rep(NA, reps),   
    "lifestage" = rep(NA, reps),   
    "media_type" = rep(NA, reps),   
    "present_in_bc_wqg" = rep(NA, reps),   
    "author" = rep(NA, reps),   
    "title" = rep(NA, reps),   
    "source" = rep(NA, reps),   
    "publication_year" = rep(NA, reps),   
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps)
  ) 
  output <- wqb_classify_duration(df, quiet = TRUE)
  expect_equal(
    output$duration_class,
#hours   1,       23,      24,     25,          50
    c("acute", "acute", "acute", "chronic", "chronic")
  )
})

# Plants ----
test_that("plant is acute under 169 hours and chronic over", {
  reps <- 8
  df <- data.frame(
    "trophic_group" = factor(rep("plant", reps)),
    "duration_hrs" = c(1, 47, 48, 49, 167, 168, 169, 500), 
    "ecological_group" = factor(rep("other", reps)), 
    "simple_lifestage" = rep(NA_character_, reps),
    "chemical_name" = rep(NA, reps),  
    "cas" = rep(NA, reps),  
    "endpoint" = rep(NA, reps),   
    "effect" = rep(NA, reps),   
    "effect_conc_mg.L" = rep(NA, reps),   
    "organism_habitat" = rep(NA, reps),   
    "species_number" = rep(NA, reps),   
    "latin_name" = rep(NA, reps),   
    "common_name" = rep(NA, reps),   
    "species_present_in_bc" = rep(NA, reps),   
    "lifestage" = rep(NA, reps),   
    "media_type" = rep(NA, reps),   
    "present_in_bc_wqg" = rep(NA, reps),   
    "author" = rep(NA, reps),   
    "title" = rep(NA, reps),   
    "source" = rep(NA, reps),   
    "publication_year" = rep(NA, reps),   
    "download_date" = rep(NA, reps),   
    "version" = rep(NA, reps)
  ) 
  output <- wqb_classify_duration(df, quiet = TRUE)
  expect_equal(
    output$duration_class,
#hours  1,        47,      48,      49,     167,     168,      169,       500
    c("acute", "acute", "acute", "acute", "acute", "acute", "chronic", "chronic")
  )
})
