<!-- NEWS.md is maintained by https://fledge.cynkra.com, contributors should not edit this file -->

# wqbench 0.2.0

- Added template for adding data to the pulled data set.
- Updated default number of bootstrap samples to 1,000.
- Updated to pull BC wqg data from `bcdata` package.
- Updated data structure to account for ECOTOX version 09-14-2023 data structure.
- Minimum version of R required is now 4.1.
- Updated internal reference data sets to ECOTOX version 03-14-2024. 
- Trophic groups reference data now includes phylum division and family.
- Filtered out `"Moss, Hornworts"`, `"Miscellaneous"`, `"Fungi"`, `"Reptiles"` from ecotox_group column in species table.
- Added developer instructions for updating reference data sets and other tasks.
- ssdtools set to use (>= 1.0.6.9011) and use stable distributions.

# wqbench 0.1.0

- First working version of the package.


# wqbench 0.0.0.9000

- Added a `NEWS.md` file to track changes to the package.
