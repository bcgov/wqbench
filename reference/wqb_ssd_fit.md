# Fit BCANZ Distributions

Wrapper to
[`ssdtools::ssd_fit_bcanz()`](https://bcgov.github.io/ssdtools/reference/ssd_fit_bcanz.html).
The sp_aggre_conc_mg.L values are the concentrations used.

## Usage

``` r
wqb_ssd_fit(data, dists = ssdtools::ssd_dists_bcanz())
```

## Arguments

- data:

  A data frame

- dists:

  A character vector of the distributions to fit

## Value

A data frame

## Examples

``` r
if (FALSE) { # \dontrun{
fit <- wqb_ssd_fit(data)
} # }
```
