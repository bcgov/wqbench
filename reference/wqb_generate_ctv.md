# Generate Critical Toxicity Value

Determine the critical toxicity value for the species. The function will
apply the method listed in the method column.

## Usage

``` r
wqb_generate_ctv(data, dists = ssdtools::ssd_dists_bcanz())
```

## Arguments

- data:

  A data frame

- dists:

  A character vector of the distributions to fit.

## Value

A data frame

## Details

The ctv_est_mg.L is the estimate of the critical toxicity value for the
species. The ctv_lcl_mg.L is the lower confidence limit. The
ctv_ucl_mg.L is the upper confidence limit.

The Deterministic method will always produce missing values in the
ctv_lcl_mg.L and ctv_ucl_mg.L columns.

## Examples

``` r
if (FALSE) { # \dontrun{
ctv <- wqb_generate_ctv(data)
} # }
```
