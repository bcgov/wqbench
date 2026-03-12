# Create the Data Set for the App

This function downloads the data, creates and adds all the data to the
database, compiles, standardizes, and classifies the data. The output of
this function is fed into the app.

## Usage

``` r
wqb_create_data_set(
  file_path = "~/Ecotoxicology/ecotox",
  version = 1,
  folder_path = "~/Ecotoxicology/ecotox_db/",
  quiet = FALSE,
  ask = TRUE,
  save_rds = TRUE
)
```

## Arguments

- file_path:

  A string of the file path location to save the downloaded files. The
  default is your current working directory.

- version:

  An integer to indicate which version you want to download. The default
  is 1 which downloads the most recent version.

- folder_path:

  Folder path to write to.

- quiet:

  Turn off message when quiet set to TRUE.

- ask:

  Turn off question when set to FALSE.

- save_rds:

  Saves the data set as a rds file in the folder_path. Turn off when set
  to FALSE.

## Value

A data frame

## Examples

``` r
if (FALSE) { # \dontrun{
wqb_create_data_set(
  file_path = "~/Ecotoxicology/ecotox",
  version = 1,
  folder_path = "~/Ecotoxicology/ecotox_db/"
)
} # }
```
