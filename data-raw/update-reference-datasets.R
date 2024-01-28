rm(list = ls())
library(tidyverse)


# Duration Conversion -----------------------------------------------------
## Read --------------------------------------------------------------------
# read in reference file
duration_std_file_path <- system.file(
  "extdata/duration-conversion.csv",
  package = "wqbench"
)

duration_std <- readr::read_csv(
  duration_std_file_path,
  show_col_types = FALSE
)

# read in reviewed and updated file
reviewed_duration_std <- list.files(
  path = file.path(
    "~",
    "Poisson",
    "Data",
    "wqbench",
    format(Sys.Date(), "%Y"),
    "review",
    "complete"
  ),
  pattern = "duration-conversion",
  full.names = TRUE
)

reviewed_duration_std <- readr::read_csv(
  reviewed_duration_std
)

## Combine Files -----------------------------------------------------------
# add new rows 
new_durations <- 
  anti_join(reviewed_duration_std, duration_std,  by = c("code")) |>
  filter(code != "-X") |> # weird issue due to excel, this row may need to be manually checked for updates
  mutate(
    duration_units_to_keep = if_else(!is.na(add_new_multipler), 1, 0),
    duration_value_multiplier_to_hours =  if_else(!is.na(add_new_multipler), add_new_multipler, NA_real_)
  )

# update any old ones
updated_durations <- 
  reviewed_duration_std %>% 
  filter(!is.na(add_new_multipler)) |>
  filter(!is.na(duration_units_to_keep)) |>
  mutate(
    duration_units_to_keep = 1,
    duration_value_multiplier_to_hours = add_new_multipler
  ) |>
  select(
    code, 
    duration_units_to_keep_2 = duration_units_to_keep, 
    duration_value_multiplier_to_hours_2 = duration_value_multiplier_to_hours,
    comments_2 = add_comments
  )

# put together
new_duration_std <- 
  left_join(duration_std, updated_durations, by = c("code")) |>
  mutate(
    duration_units_to_keep = if_else(!is.na(duration_units_to_keep_2), duration_units_to_keep_2, duration_units_to_keep),
    duration_value_multiplier_to_hours = if_else(!is.na(duration_value_multiplier_to_hours_2), duration_value_multiplier_to_hours_2, duration_value_multiplier_to_hours),
    comments = if_else(!is.na(comments_2), comments_2, comments)
  ) |>
  bind_rows(new_durations) |>
  select(
    code, 
    description,
    duration_units_to_keep,
    duration_value_multiplier_to_hours,
    comments
  ) |>
  mutate(
    duration_units_to_keep = as.logical(duration_units_to_keep)
  )

## Write new reference file ------------------------------------------------
write_csv(
  new_duration_std,
  "inst/extdata/duration-conversion.csv",
)
