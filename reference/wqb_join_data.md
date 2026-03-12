# Join Data

Join database tables together and start filtering and cleaning data.

## Usage

``` r
wqb_join_data(database, quiet = FALSE)
```

## Arguments

- database:

  A string to the location of the database.

- quiet:

  Turn off message when quiet set to TRUE.

## Value

Invisible data frame

## Details

Check the resource document more details on the data added, filter
conditions and cleaning steps. This is part of Step 1.

## Examples

``` r
if (FALSE) { # \dontrun{
data_compiled <- wqb_join_data(
  database = "ecotox_ascii_09_15_2022.sqlite"
)

data_compiled <- wqb_join_data(
  database = "ecotox_db/ecotox_ascii_09_15_2022.sqlite"
)
} # }
```
