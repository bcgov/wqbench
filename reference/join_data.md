# Join Data

Internal to wqb_join_data() to allow for testing.

## Usage

``` r
join_data(
  db_results,
  db_tests,
  db_endpoint_codes,
  db_species,
  db_lifestage_codes,
  db_chemicals,
  db_duration_unit_codes,
  db_concentration_unit_codes,
  db_references,
  db_effect_codes,
  db_media_type_codes,
  db_meta_data_download
)
```

## Arguments

- db_results:

  A data frame

- db_tests:

  A data frame

- db_endpoint_codes:

  A data frame

- db_species:

  A data frame

- db_lifestage_codes:

  A data frame

- db_chemicals:

  A data frame

- db_duration_unit_codes:

  A data frame

- db_concentration_unit_codes:

  A data frame

- db_references:

  A data frame

- db_effect_codes:

  A data frame

- db_media_type_codes:

  A data frame

- db_meta_data_download:

  A data frame

## Value

Invisible data frame

## Examples

``` r
if (FALSE) { # \dontrun{
data <- wqb_join_data(
  db_results, db_tests, db_endpoint_codes, db_species, db_lifestage_codes,
  db_chemicals, db_duration_unit_codes, db_concentration_unit_codes,
  db_references, db_effect_codes, db_media_type_codes, db_meta_data_download
)
} # }
```
