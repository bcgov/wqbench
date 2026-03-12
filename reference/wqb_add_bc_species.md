# Add BC Species

Read in a list of British Columbia species and add a column to the
`species` table in the database to indicate if the species is present in
British Columbia.

## Usage

``` r
wqb_add_bc_species(database, quiet = FALSE)
```

## Arguments

- database:

  A string to the location of the database.

- quiet:

  Turn off message when quiet set to TRUE.

## Value

Invisible data frame

## Details

The BC species data is contained in a csv file in the extdata folder of
the package. This file can be edited by adding new species or removing
species. Do not add new columns, rename columns or rename the file. The
file must only contain a single column named `latin_name`.

The `latin_name` column must consist of the genus and species separated
by a space. The `latin_name` column in the bc-species.csv file is
matched to the `latin_name` column in the species table of the database.
A new column `species_present_in_bc` is added to the species table that
codes each species as TRUE if it matches a value in the bc-species.csv
or FALSE if there is no match.

## Examples

``` r
if (FALSE) { # \dontrun{
bc_species <- wqb_add_bc_species(
  database = "ecotox_ascii_09_15_2022.sqlite"
)

bc_species <- wqb_add_bc_species(
  database = "ecotox_db/ecotox_ascii_09_15_2022.sqlite"
)
} # }
```
