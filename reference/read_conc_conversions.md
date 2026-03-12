# Read Concentration Unit Conversation Values

Internal to allow for testing

## Usage

``` r
read_conc_conversions(concentration_std_file_path, db_concentration_unit_codes)
```

## Arguments

- concentration_std_file_path:

  A file path

- db_concentration_unit_codes:

  A data frame

## Value

A data frame

## Examples

``` r
if (FALSE) { # \dontrun{
concentration_unit_codes_std <- read_conc_conversions(
  concentration_std_file_path, db_concentration_unit_codes
)
} # }
```
