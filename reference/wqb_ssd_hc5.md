# Run SSD to get Hazard Concentrations

Wrapper to the
[`ssdtools::ssd_hc_bcanz()`](https://bcgov.github.io/ssdtools/reference/ssd_hc_bcanz.html)
function and selects only the row which is 5%.

## Usage

``` r
wqb_ssd_hc5(fit, nboot = 1000)
```

## Arguments

- fit:

  The fit from ssd

- nboot:

  A count of the number of bootstrap samples to use to estimate the SE
  and confidence limits. Default value of 1000.

## Value

A data frame

## Details

The number of bootstrap samples is set to 1000 so the estimates are
quick to generate. Check out
[`ssdtools::ssd_hc_bcanz()`](https://bcgov.github.io/ssdtools/reference/ssd_hc_bcanz.html)
for more details.

## Examples

``` r
if (FALSE) { # \dontrun{
hc5 <- wqb_ssd_hc5(fit)
} # }
```
