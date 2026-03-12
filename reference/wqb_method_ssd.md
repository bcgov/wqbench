# SSD Method to Calculate Critical Toxicity Value for the Chemical

Use a species sensitivity distribution to calculate the critical
toxicity value for the data set. The critical toxicity value is the
hazardous concentration for 5% of species (HC5) when the SSD method is
used.

## Usage

``` r
wqb_method_ssd(data, fit, nboot = 1000)
```

## Arguments

- data:

  A data frame

- fit:

  The fit

- nboot:

  The number of bootstrap samples. Default value of 1000.

## Value

A data frame

## Details

A wrapper on the ssdtools package function
[`ssdtools::ssd_hc_bcanz()`](https://bcgov.github.io/ssdtools/reference/ssd_hc_bcanz.html)
that only returns the HC5 concentration.

## Examples

``` r
if (FALSE) { # \dontrun{
hc5 <- wqb_method_ssd(data, fit)
} # }
```
