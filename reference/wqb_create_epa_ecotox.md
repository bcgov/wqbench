# Create the ECOTOX Database

Create a SQLite database from the US EPA ECOTOX downloaded files.

## Usage

``` r
wqb_create_epa_ecotox(folder_path = ".", data_path, quiet = FALSE, ask = TRUE)
```

## Arguments

- folder_path:

  Folder path to write to.

- data_path:

  Folder path to the downloaded ECOTOX folder

- quiet:

  Turn off message when quiet set to TRUE.

- ask:

  Turn off question when set to FALSE.

## Value

Invisible string of the file path of the database.

## Details

This functions reads in the text files in the folder and writes them to
the database.

This function will overwrite a database if already present.

## Examples

``` r
if (FALSE) { # \dontrun{
wqb_create_epa_ecotox(data_path = "ecotox_ascii_12_15_2022")
} # }
```
