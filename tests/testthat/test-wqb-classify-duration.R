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





