Developer Instructions
================

<!--
rmarkdown::html_vignette:
rmarkdown::github_document:
&#10;rmarkdown::render("vignettes/Developer-instructions.Rmd")
-->

``` r
library(wqbench)
```

## Updating the Internal Data Set in the Shiny app

The shiny app is located in the
[shinywqbench](https://github.com/bcgov/shinywqbench) repository. To
update the internal data file

1.  Go to the `inst/extdata/data.R` file.
2.  Run the script.

This needs to occur each time the ECOTOX EPA data set is updated on the
website.

## How to Deploy the Shiny App

The shiny app is located in the
[shinywqbench](https://github.com/bcgov/shinywqbench) repository.

1.  Go to the scripts/deploy.R file.
2.  Run the script.

It is advised to first run the code that deploys the app name of
`shinywqbench-dev` and confirm the app deploys and functions as
expected. This app is referred to as the development app.

Once it has been confirmed that the app functions and deploys properly
to then run the second chunk of code with the app name `shinywqbench`.
This app is referred to as the production app.

This will help to ensure the production app is always in a working
state.

## Updating Reference Data for the Database

Each time the Ecotox database is updated, the reference files in the
package should be reviewed for necessary changes. This process involves
generating some files to illustrate changes, having those files reviewed
by an expert, and updated if necessary, and then updating the data files
in the package with the new changes.

These changes may involve:

- concentration conversions and duration conversions for newly-added
  concentration and duration units,
- trophic groups and life stage codes for newly added species.

Lists of BC species and concentration endpoints are unlikely to require
changes but should be reviewed.

### Process

1.  Ensure you have the most recent copy of the database with the
    reference files added. To do this run the function
    `wqb_create_data_set()`.

``` r
wqb_create_data_set(
  file_path = "~/Ecotoxicology/ecotox",
  version = 1,
  folder_path = "~/Ecotoxicology/ecotox_db/"
)
```

2.  Execute the code in the script
    *scripts/review-reference-datasets-01.R*.

    - This script will generate and save a set of csv files that need to
      be reviewed and updated to allow new values through the data
      cleaning steps.
    - At the top of the script you will need to set the file path for
      the database and the location to save the files that are
      generated.

3.  After the files are generated, they need to be reviewed and updated
    by a technical expert.

    - It is recommended to email them to the appropriate person for
      their review.
    - The next step must wait until the review is complete.
    - The life stage code file can’t be reviewed until after the trophic
      groups have been updated.

4.  Once the files are reviewed the
    *script/update-reference-datasets-01.R* script needs to be run.

    - This will read in the reviewed files and update the reference
      files in the *inst/extdata* folder.

5.  The package needs to be re-built for the files to be part of the
    package.

6.  Run the `wqb_create_data_set()` function to create the database with
    the new reference data.

7.  Repeat steps 2 through 6 but run
    *scripts/review-reference-datasets-02.R* and
    *script/update-reference-datasets-02.R*.

    - The life stage codes have to be generated seperately as they are
      based on the trophic groups. The trophic groups need to be updated
      before life stage codes can be reviewed.

Below are instructions for how to fill out and complete each of the
reference files.

#### Responsibilities

Programmer/Analyst

- Generate reference files.
- Send to technical expert for review.
- Receive updated reference files.
- Review updates to ensure they are compliant with rules.
  - Make sure checks pass in *update-reference-dataset.R* script.
  - Review all changes generated in the daff reports.
  - Communicate with technical expert as needed.
- Update package with new reference data.
- Check reference data can be integrated into the ECOTOX data.

Technical Expert

- Review and update reference files.
- Follow instructions for how to fill out and complete updates.
- Send reviewed/updated reference files back to programmer.

### Concentration Conversion

This data set should be reviewed each time a new version of the ECOTOX
database is released.

- If there is no value in the **conc_conversion_flag** column this
  indicates it is a new concentration unit that was not in the previous
  version of the database.
- The goal of the review is to ensure all cells in the
  **conc_conversion_flag** column are filled in.
  - A zero (`0`) indicates the concentration cannot be converted to mg/L
    or ppm.
    - If the units cannot be converted to mg/L or ppm then put a `0` in
      the row.
    - No additional columns need to be filled in if the unit is given a
      value of `0` in the **conc_conversion_flag** column.
  - A one (`1`) indicates the concentration can be converted to mg/L or
    ppm.
    - If the units can be converted put a `1` in the row.
      - In the **conc_conversion_value_multiplier** column fill in the
        value needed to convert the units into mg/L or ppm.
      - In the **conc_conversion_unit** column fill in either `mg/L` or
        `ppm`.

If any incorrect conversions are found, then those rows can be updated.

### Duration Conversion

This data set should be reviewed each time a new version of the ECOTOX
database is released.

- If there is no value in the **duration_units_to_keep** column this
  indicates it is a new concentration unit that was not in the previous
  version of the database.
- The goal of the review is to ensure all cells in the
  **duration_units_to_keep** column are filled in.
  - A zero (`0`) indicates the duration cannot be converted to hours.
    - If the units cannot be converted to hours, then put a `0` in the
      row.
    - No additional columns need to be filled in if the unit is given a
      value of `0` in the **duration_units_to_keep** column.
  - A one (`1`) indicates the duration can be converted to hours.
    - If the units can be converted put a `1` in the row.
      - In the **duration_value_multiplier_to_hours** column fill in the
        value needed to convert the units into hours.
      - In the **comments** column write why the conversion was chosen
        since there may be an assumption made during the conversion. For
        example, if converting month into hours are you basing the
        conversion on 30 or 31 days.

If any incorrect conversions are found, then those rows can be updated.

### Trophic Groups

This data set should be reviewed each time a new version of the ECOTOX
database is released.

- Three files will be generated to help review the trophic group data.
  - *trophic-group.csv*
    - This is the current list of trophic and ecological groups coded to
      the class and order of each species.
    - This file needs to be updated to add new trophic and ecological
      groups.
    - To update add a new row and fill in the **trophic_group** and
      **ecological_group** columns and the required **class** and
      **order**.
      - The **class** and **order** can either both be filled in or one
        of the values left blank.
      - The **class** and **order** columns must match up with the
        **class** and **tax_order** columns from the
        *species-coded-in-db.csv* or *missing-trophic-group.csv* file.
  - *species-coded-in-db.csv*
    - This is a list of all the species data from the database that have
      been filter where **organism_habitat** is “Water”.
    - This file is to help find which **class** and **tax_order**
      (order) do not have coding for the ecological or trophic groups.
      - Filter to rows that have no value in the **trophic_group** and
        **ecological_group** columns.
    - This file is for reference and not to be updated or sent back for
      integration.
    - This data is summarized in the *missing-trophic-group-review.csv*
      file because of the large amount of information.
  - *missing-trophic-group-review.csv*
    - This is a summary that shows only the unique phylum, class, order,
      and family from the missing trophic and ecological groups in the
      *species-coded-in-db.csv* file.
    - This file will help to determine which class and order need to be
      added.
    - This file is for reference and not to be updated or sent back for
      integration.

If any incorrect values are found, then those rows can be updated.

### Life Stage Codes

This data set should be reviewed each time a new version of the ECOTOX
database is released.

This data set will be sent separately after the first round of files is
reviewed because this data depends on the updates to the trophic group
data.

- If there is no value in the **simple_lifestage** column this indicates
  it is a new life stage that was not in the previous coding of the
  database.
- The **trophic_group** column indicates if the **simple_lifestage**
  relates to a fish or amphibian. Only fish and amphibian groups need
  the life stage categorized into simple groups.
- The goal of the review is to ensure all cells in the
  **simple_lifestage** column that have **fish_amphibian_flag** column
  as TRUE are filled in.
  - In the **simple_lifestage** column fill in the empty cell with
    either `els` (early life stage), `juvenile` or `adult`.

If any incorrect values are found, then those rows can be updated.

### BC Species

This reference dataset has not been included in the
*review-reference-datasets.R* script as it should not vary from year to
year.

This data set is a comprehensive list that was generated from B.C.
Conservation Data Centre.

Any species that is not listed in the *bc-species.csv* reference file is
marked as not present in BC and thus it is robust to new species added
to the ECOTOX dataset.

### Concentration Endpoints

This reference dataset has not been included in the
*review-reference-datasets.R* script as it should not vary from year to
year.

There is a comprehensive list that should not need to be adjusted.

The list of concentration endpoints are generated in the
*scripts/concentration-endpoints.R* script.

If updates are required then update the *concentration-endpoints.R*
script.

## Updating the Add Data Template

#### Associated Reference Data

The instructions in the upload data template need to be updated when
changes to the reference data sheets occur.

- *trophic-groups.csv*
  - Update **trophic_group** and **ecological_group** column
    instructions if new groups are added or groups are removed.
    - ie if `Plant` is removed as a trophic group or `Salmonid` is
      removed as an ecological group.
    - Since the groups (Plant, Bacteria, Amphibian, etc) have been
      written into the instruction tab this is why updates to the
      instructions are needed if new categories are added.
  - The validation of the values is done from the *trophic-groups.csv*
    which means it is possible for the instructions to get out of sync
    with the allowed values.
  - Adding new species to the *trophic-groups.csv* does not trigger a
    change to the instructions.
- *lifestage-codes.csv*
  - Update **simple_lifestage** column instructions if new simple life
    stages categories are added or removed.
    - ie if `els` is removed as a simple life stage.
  - The validation is done off the *lifestage-codes.csv* which means it
    is possible for the instructions to get out of sync with the allowed
    values.
- *concentration-endpoints.csv*
  - Update **endpoint** column instructions if endpoints are added or
    removed.
  - The list of endpoints are generated by the script
    *scripts/concentration-endpoints.R*.
  - The validation is done off of the *concentration-endpoints.csv* so
    it is possible for the instructions to get out of sync with the
    allowed values.

#### To update the template

1.  Make any required changes to the trophic groups, life stage codes or
    concentration endpoints reference data.
2.  Edit the *inst/template/template-data-data.csv* and
    *inst/template/template-data-instructions.csv* files.
    - Do not edit the file *inst/template/template-data.xlsx*.
3.  To regenerate the template run the script *data-raw/template.R*.
4.  Review the *inst/template/template-data.xlsx* to confirm the edits
    came through and the template looks as expected.
    - Do not edit the file *inst/template/template-data.xlsx*.
