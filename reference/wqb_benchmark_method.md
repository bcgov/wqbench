# Determine Benchmark Method

Determine if a species sensitivity distribution (SSD) or deterministic
method will be used to determine the aquatic life water quality
benchmark value for the data. A column called `method` will be added to
the data indicating which method will be used.

## Usage

``` r
wqb_benchmark_method(data)
```

## Arguments

- data:

  A data frame

## Value

A data frame

## Details

Check the resource document for the rules used to determine which
benchmark method will be used. This is Step 4.

## Examples

``` r
if (FALSE) { # \dontrun{
data <- wqb_benchmark_method(data)
data <- wqb_benchmark_method(aggregated_data)
} # }
```
