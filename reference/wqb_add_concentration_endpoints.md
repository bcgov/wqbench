# Add the Concentration Endpoints

Read in the concentration endpoints that have been selected to keep in
the final data set. Add a column to the endpoint_codes table in the
database to mark the corresponding endpoints.

## Usage

``` r
wqb_add_concentration_endpoints(database, quiet = FALSE)
```

## Arguments

- database:

  A string to the location of the database.

- quiet:

  Turn off message when quiet set to TRUE.

## Value

Invisible data frame

## Details

The list of concentration endpoints is contained in a csv file in the
extdata folder of the package. The csv file can be edited by adding or
removing rows. To add new rows get the `code` and `description` values
from the `endpoint_codes` table in the database and paste them into the
csv file.

Do not add new columns, rename columns or rename the file. The file must
only contain the `code` and `description` column.

The `code` values in the endpoint-concentration file are matched to the
`code` values in the endpoint_code table in the database. A new column
`concentration_flag` is added to the endpoint_code table that codes each
endpoint as TRUE if the endpoint is present in
endpoint-concentration.csv file.

## Examples

``` r
if (FALSE) { # \dontrun{
wqb_add_concentration_endpoints(
  database = "ecotox_ascii_09_15_2022.sqlite"
)

wqb_add_concentration_endpoints(
  database = "ecotox_db/ecotox_ascii_09_15_2022.sqlite"
)
} # }
```
