# Add Life Stage Groups

Read in the life stage simple groups and add a column in the
lifestage_code table in the database to mark the corresponding values.

## Usage

``` r
wqb_add_lifestage(database, quiet = FALSE)
```

## Arguments

- database:

  A string to the location of the database.

- quiet:

  Turn off message when quiet set to TRUE.

## Value

Invisible data frame

## Details

Only life stages related to fish or amphibians have been coded. The
purpose of the coding is to be able to simplify the many life stages
into three categories: els (early life stage), juveniles and adults. Not
all life stages have been coded into these three groups.

The life stage data is contained in a csv file in the extdata folder of
the package. The csv file can be edited by adding or removing rows. To
add new rows get the `code` and `description` values from the
`lifestage_code` table in the dataset and paste them into the csv file
and then add the value to the `simple_lifestage` column.

Do not add new columns, rename columns or rename the file. The file must
only contain the `code`, `description_lifestage` and `simple_lifestage`
column.

The `code` values in the lifestage-codes.csv file are matched to the
`code` values in the lifestage_code table in the database. Any codes
that match are coded in a new column called `simple_lifestage`.

## Examples

``` r
if (FALSE) { # \dontrun{
lifestage_codes <- wqb_add_lifestage(
  database = "ecotox_ascii_09_15_2022.sqlite"
)

lifestage_codes <- wqb_add_lifestage(
  database = "ecotox_db/ecotox_ascii_09_15_2022.sqlite"
)
} # }
```
