# Determine the Assessment Factors

Determines the species variation factor, ecological assessment factor
and B.C. species factor.

## Usage

``` r
wqb_af(data)
```

## Arguments

- data:

  A data frame

## Value

A data frame

## Details

This is a wrapper function that calls
[`wqb_af_bc_species()`](https://bcgov.github.io/wqbench/reference/wqb_af_bc_species.md),
[`wqb_af_ecological()`](https://bcgov.github.io/wqbench/reference/wqb_af_ecological.md)
and
[`wqb_af_variation()`](https://bcgov.github.io/wqbench/reference/wqb_af_variation.md).

## Examples

``` r
if (FALSE) { # \dontrun{
data <- wqb_af(data)
} # }
```
