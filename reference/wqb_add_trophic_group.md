# Add Trophic Groups to Database

Read in the trophic and ecological groups and and add a column to the
`species` table in the database that lists the groups for each species.

## Usage

``` r
wqb_add_trophic_group(database, quiet = FALSE)
```

## Arguments

- database:

  A string to the location of the database.

- quiet:

  Turn off message when quiet set to TRUE.

## Value

Invisible data frame

## Details

The trophic group data is contained in a csv file in the extdata folder
of the package. This file can be edited by adding or removing groups and
classes. Do not add new columns, rename columns or rename the file.

The trophic groups file must contain the columns: `class`, `order`,
`trophic_group`, and `ecological_group`. The `class` and `order` columns
are matched to the `class` and `tax_order` columns in the `species`
table of the database and then adds the `trophic_group` and
`ecological_group` columns to the species table.

## Examples

``` r
if (FALSE) { # \dontrun{
trophic_group <- wqb_add_trophic_group(
  database = "ecotox_ascii_09_15_2022.sqlite"
)

trophic_group <- wqb_add_trophic_group(
  database = "ecotox_db/ecotox_ascii_09_15_2022.sqlite"
)
} # }
```
