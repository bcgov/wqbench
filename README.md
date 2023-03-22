
# wqbench

<!-- badges: start -->

[![img](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://github.com/bcgov/repomountie/blob/master/doc/lifecycle-badges.md)
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
duration_unit_code_standardization <- wqb_add_duration_conversions(database = database)
concentration_unit_code_standardization <- wqb_add_conc_conversions(database = database)
data <- wqb_compile_dataset(database = database) 
```

``` r
data <- wqb_classify_duration(data)
data <- wqb_standardize_effect(data)
data <- wqb_filter_chemical(data, "129909906")
data <- wqb_benchmark_method(data)
data <- wqb_aggregate(data)
data <- wqb_af_variation(data)
data <- wqb_af_ecological(data)
data <- wqb_af_bc_species(data)
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

If the data uses the deterministic method to calculate the benchmark
value

``` r
gp <- wqb_plot_det(data_agg)
```

If the data uses the ssd method to calculate the benchmark value

``` r
gp <- wqb_plot_ssd(data_agg)
```

## Getting Help or Reporting an Issue

To report issues, bugs or enhancements, please file an
[issue](https://github.com/bcgov/wqbench/issues). Check out the
[support]() for more info.

## Code of Conduct

Please note that the wqbench project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## License

The code is released under the Apache License 2.0

Copyright 2023 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the “License”); you may
not use this file except in compliance with the License. You may obtain
a copy of the License at

<https://www.apache.org/licenses/LICENSE-2.0>

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an “AS IS” BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
