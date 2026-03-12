# Add Media Type Coding of Salt or Fresh Water

Code the media type information into three categories: salt water, fresh
water and not reported.

## Usage

``` r
wqb_add_media(database, quiet = FALSE)
```

## Arguments

- database:

  A string to the location of the database.

- quiet:

  Turn off message when quiet set to TRUE.

## Value

Invisible data frame

## Details

Read in the media_type_codes table from the database and code the 24
groups into three categories: salt water, fresh water or not reported.
The new categories are added to a column in the table called
`media_type_group`.

## Examples

``` r
if (FALSE) { # \dontrun{
media_info <- wqb_add_media(
  database = "ecotox_ascii_09_15_2022.sqlite"
)

media_info <- wqb_add_media(
  database = "ecotox_db/ecotox_ascii_09_15_2022.sqlite"
)
} # }
```
