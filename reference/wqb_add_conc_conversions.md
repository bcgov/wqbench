# Add Concentration Unit Conversation Values

Read in the concentration-conversion file and code units that can be
converted and which ones will be removed. Three columns will be added to
the concentration_unit_codes table in the database, indicate which units
are being kept, the conversion factor, and the units it is being
converted to. Currently units are converted to mg/L or equivalent.

## Usage

``` r
wqb_add_conc_conversions(database, quiet = FALSE)
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
from the `concentration_unit_codes` table in the database and paste them
into the csv file.

Do not add new columns, rename columns or rename the file. The file must
only contain the columns: `code`, `description`, `conc_conversion_flag`,
`conc_conversion_value_multiplier` and `conc_conversion_unit`.

The `conc_conversion_flag` column indicates which units are being
converted and which are being removed because they can not be converted
or are not in the aquatic portion of the data.The
`conc_conversion_value_multiplier` column contains the value need to
convert the unit into the unit listed in the `conc_conversion_unit`
column.

The `code` values in the concentration-conversion file are matched to
the `code` values in the `concentration_unit_codes` table in the
database.

## Examples

``` r
if (FALSE) { # \dontrun{
concentration_unit_code_standardization <- wqb_add_conc_conversions(
  database = "ecotox_ascii_09_15_2022.sqlite"
)

concentration_unit_code_standardization <- wqb_add_conc_conversions(
  database = "ecotox_db/ecotox_ascii_09_15_2022.sqlite"
)
} # }
```
