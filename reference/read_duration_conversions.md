# Read Duration Unit Conversation

Internal to allow for testing

## Usage

``` r
read_duration_conversions(duration_std_file_path, db_duration_unit_codes)
```

## Arguments

- duration_std_file_path:

  A file path

- db_duration_unit_codes:

  A data frame

## Value

A data frame

## Examples

``` r
if (FALSE) { # \dontrun{
duration_unit_codes_std <- read_duration_conversions(
  duration_std_file_path, db_duration_unit_codes
)
} # }
```
