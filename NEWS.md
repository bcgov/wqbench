<!-- NEWS.md is maintained by https://fledge.cynkra.com, contributors should not edit this file -->

# wqbench 0.3.2

- Internal changes. 

# wqbench 0.3.1

- Fixed bug in `wqb_check_add_data()` that was not allowing the "Planktonic Invertebrates" as an ecological group. 

# wqbench 0.3.0

- fixed `read_bcg()` to work with updated data in BC Data Catalogue (#46)
- Updated Ecotox version to 09-12-2024 and internal reference files (#49)
- Updated BC Species list (#50)
- Updated developer documentation (#47, #51)
  - Separate _package_ update instructions from _app_ update instructions
  - Enhanced update instructions
  - Trophic group review: started a list of taxa to include based on ecotox group, and exclude based on reviews. This means that reviews won't be overwhelmed by huge lists of taxonomic groups that are not relevant.
  - Made reading in large csvs more robust by explicitly adding column types
- Updated minimum ssdtools package version

# wqbench 0.2.0

- Added template for adding data to the pulled data set.
- Updated default number of bootstrap samples to 1,000.
- Updated to pull BC wqg data from `bcdata` package.
- Updated data structure to account for ECOTOX version 09-14-2023 data structure.
- Minimum version of R required is now 4.1.
- Updated internal reference data sets to ECOTOX version 12-14-2023.
- Trophic groups reference data now includes phylum division and family.
- Filtered out `"Moss, Hornworts"`, `"Miscellaneous"`, `"Fungi"`, `"Reptiles"` from ecotox_group column in species table.
- Added developer instructions for updating reference data sets and other tasks.
- ssdtools set to use (>= 1.0.6.9011) and use stable distributions.

# wqbench 0.1.0

- First working version of the package.


# wqbench 0.0.0.9000

- Added a `NEWS.md` file to track changes to the package.
