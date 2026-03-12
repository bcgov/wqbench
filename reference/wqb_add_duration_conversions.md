# Add Duration Unit Conversation Values

Read in the duration-conversion file and code unit values that can be
converted and which ones will be removed. Two columns will be added to
the duration_unit_codes table in the database to indicate which units
are being kept and the conversion factor. Currently duration units are
converted to hours.

## Usage

``` r
wqb_add_duration_conversions(database, quiet = FALSE)
```

## Arguments

- database:

  A string to the location of the database.

- quiet:

  Turn off message when quiet set to TRUE.

## Value

Invisible data frame

## Details

The list of units to be converted are contained in a csv file in the
extdata folder of the package. The csv file can be edited by adding or
removing rows. To add new rows get the `code` and `description` values
from the `duration_unit_codes` table in the database and paste them into
the csv file.

Do not add new columns, rename columns or rename the file. The file must
only contain the columns: `code`, `description`,
`duration_units_to_keep` and `duration_value_multiplier_to_hours`.

The `code` values in the duration-conversion file are matched to the
`code` values in the `duration_unit_codes` table in the database.

The `duration_units_to_keep` column indicates which units are being
converted and which are being removed because they can not be converted
or are not in the aquatic portion of the data. The
`duration_value_multiplier_to_hours` column contains the value need to
convert the unit into hours.

## Examples

``` r
if (FALSE) { # \dontrun{
duration_unit_code_standardization <- wqb_add_duration_conversions(
  database = "ecotox_ascii_09_15_2022.sqlite"
)

duration_unit_code_standardization <- wqb_add_duration_conversions(
  database = "ecotox_db/ecotox_ascii_09_15_2022.sqlite"
)
} # }
```
