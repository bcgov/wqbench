# Add BC Water Quality Guidelines

Read in the British Columbia water quality guidelines (wqg) and add a
column to the chemicals tables in the database to indicate if the
chemical is present in the British Columbia water quality guidelines.

## Usage

``` r
wqb_add_bc_wqg(database, quiet = FALSE)
```

## Arguments

- database:

  A string to the location of the database.

- quiet:

  Turn off message when quiet set to TRUE.

## Value

Invisible data frame

## Details

The wqg data is stored in the BC Data Catalogue.

The `CAS_number` column in the bc wqg data is matched to the
`cas_number` column in the chemicals table of the database. A new column
`present_in_bc_wqg` is added to the chemicals table that codes each
chemical as TRUE if the chemical is present in wqg or FALSE if the
chemical is not present in wqg.

## Examples

``` r
if (FALSE) { # \dontrun{
chem_bc_wqg <- wqb_add_bc_wqg(
  database = "ecotox_ascii_09_15_2022.sqlite"
)

chem_bc_wqg <- wqb_add_bc_wqg(
  database = "ecotox_db/ecotox_ascii_09_15_2022.sqlite"
)
} # }
```
