
# wqbench

<!-- badges: start -->
<!-- badges: end -->

The goal of wqbench is to …

## Installation

``` r
# install.packages("devtools")
devtools::install_github("poissonconsulting/wqbench")
```

## Workflow

This is a basic example which shows you how to solve a common problem:

``` r
library(wqbench)
```

### Compile Data Set

Download the most recent database

``` r
wqb_download_epa_ecotox(file_path = "~/Ecotoxicology/ecotox", version = 2)
```

Create a SQLite database from the Downloaded Ecotox Data

``` r
database <- wqb_create_epa_ecotox(
  file_path = "~/Ecotoxicology/ecotox_db/",
  data_path = "~/Ecotoxicology/ecotox/ecotox_ascii_09_15_2022"
)
```

Add and update database

- all these functions need to be run for the data set to compile.

``` r
bc_species <- wqb_add_bc_species(database = database) 
chem_bc_wqg <- wqb_add_bc_wqg(database = database)
conc_endpoints <- wqb_add_concentration_endpoints(database = database)
lifestage_codes <- wqb_add_lifestage(database = database) 
media_groups <- wqb_add_media(database = database)
trophic_groups <- wqb_add_trophic_group(database = database) 
duration_unit_code_standardization <- wqb_standardize_duration(database = database)
concentration_unit_code_standardization <- wqb_add_conc_conversions(database = database)
data <- wqb_compile_dataset(database = database) 
```

``` r
data <- wqb_classify_duration(data)
data <- wqb_standardize_effect(data)
data <- wqb_filter_chemical(data, "129909906")
data_agg <- wqb_aggregate(data)
data_agg <- wqb_benchmark_method(data_agg)
data_agg <- wqb_af_variation(data_agg)
data_agg <- wqb_af_ecological(data_agg)
data_agg <- wqb_af_bc_species(data_agg)
```

# Generate Benchmark

``` r
benchmark <- wqb_generate_bench(data_agg)
```

# Plots

Plot data set

``` r
gp <- wqb_plot(data)
```

If the data uses the vf method to calculate the benchmark value

``` r
gp <- wqb_plot_vf(data_agg)
```

If the data uses the ssd method to calculate the benchmark value

``` r
gp <- wqb_plot_ssd(data_agg)
```
