# Plot Species Sensitivity Distribution

Plot results to see the species sensitivity distribution. The dashed
line shows the HC5 value.

## Usage

``` r
wqb_plot_ssd(data, fit)
```

## Arguments

- data:

  A data frame

- fit:

  The fit from ssd

## Details

This is a wrapper on
[`ssdtools::predict()`](https://rdrr.io/r/stats/predict.html) and
[`ssdtools::ssd_plot()`](https://bcgov.github.io/ssdtools/reference/ssd_plot.html).

## Examples

``` r
if (FALSE) { # \dontrun{
wqb_plot_ssd(data, fit)
} # }
```
