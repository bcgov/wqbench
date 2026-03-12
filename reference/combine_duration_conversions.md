# Combine Duration Unit Conversation Values and DB duration unit

Internal to allow for testing

## Usage

``` r
combine_duration_conversions(duration_std, db_duration_unit_codes)
```

## Arguments

- duration_std:

  A data frame

- db_duration_unit_codes:

  A data frame

## Value

A data frame

## Examples

``` r
if (FALSE) { # \dontrun{
duration_unit_codes_std <- combine_duration_conversions(
  duration_std, db_duration_unit_codes
)
} # }
```
