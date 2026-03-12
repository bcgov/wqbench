# Check the Uploaded Data

Checks the uploaded data for the basic requirements to ensure the data
matches the downloaded Ecotox data.

## Usage

``` r
wqb_check_add_data(data, template)
```

## Arguments

- data:

  A data frame. The data you want to check.

- template:

  A data frame. The format the data should be in, in the
  [chktemplate](https://poissonconsulting.github.io/chktemplate/)
  format.

## Value

A data frame

## Details

The values for the endpoint, trophic_group, and ecological_group columns
are checked against the data tables used to build the database. To
update the allowed values the corresponding csv file needs to be
updated.

## Examples

``` r
if (FALSE) { # \dontrun{
data <- wqb_check_add_data(data, template)
} # }
```
