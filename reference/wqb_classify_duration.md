# Classify Duration

Classify each test as either acute or chronic in the `duration_class`
column.

## Usage

``` r
wqb_classify_duration(data, quiet = FALSE)
```

## Arguments

- data:

  A data frame

- quiet:

  Turn off message when quiet set to TRUE.

## Value

A data frame

## Details

Check the resource document for the rules used to determine the
classification for each trophic group. This is Step 2.

## Examples

``` r
if (FALSE) { # \dontrun{
classified_data <- wqb_classify_duration(compiled_data)
} # }
```
