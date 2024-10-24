Developer Instructions
================

<!--
rmarkdown::html_vignette:
rmarkdown::github_document:
&#10;rmarkdown::render("vignettes/Developer-instructions.Rmd")
-->

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
changes but should be reviewed periodically.

### Process

0.  Download (clone or pull) the `wqbench` repo and open in RStudio. The
    best practice is to open a new git branch, called something like
    `update-[date-of-new-ecotox]`. In Rstudio, git pane -\> New branch
    -\> give it a name, and check “Sync branch with remote”.

1.  Ensure you have the most recent copy of the database with the
    reference files added. To do this, load the package, and run the
    function `wqb_create_data_set()`:

``` r
devtools::load_all()

wqb_create_data_set(
  file_path = "~/Ecotoxicology/ecotox",
  version = 1,
  folder_path = "~/Ecotoxicology/ecotox_db/"
)
```

2.  Execute the code in the script
    *scripts/review-reference-datasets-01.R*.

- This script will generate and save a set of csv files that need to be
  reviewed and updated to allow new values through the data cleaning
  steps.
- At the top of the script you will need to set the file path for the
  database and the location to save the files that are generated. By
  defeault they will save at:
  `"~/Poisson/Data/wqbench/[current-year]/review/to-be-reviewed/"`

3.  After the files are generated, they need to be [reviewed and
    updated](#review-process) by a technical expert.

- The next step must wait until the review is complete.
- The life stage code file can’t be reviewed until after the trophic
  groups have been updated.

4.  Once the files are reviewed they should be placed in:
    `"~/Poisson/Data/wqbench/[current-year]/review/completed/"`

- Then run the *scripts/update-reference-datasets-01.R* script.
- Run this line by line - You will be shown several displays showing the
  updates that will be made to the internal data - ensure these look ok.
  If they do, continue executing the script, if not you will need to
  revisit your review and the changes you made to the csv files in the
  `completed` folder.
- This will read in the reviewed files and update the reference files in
  the *inst/extdata* folder.

5.  The package needs to be re-built for the files to be part of the
    package. Run `devtools::load_all()`.

6.  Run the `wqb_create_data_set()` function to create the database with
    the new reference data.

7.  Repeat steps 2 through 6 but run
    *scripts/review-reference-datasets-02.R*, review the file, and run
    *scripts/update-reference-datasets-02.R*.

- The life stage codes have to be generated seperately as they are based
  on the trophic groups. The trophic groups need to be updated before
  life stage codes can be reviewed.

8.  Run the `wqb_create_data_set()` function again to create the
    database with the new reference data.

9.  These steps will have caused changes to several files in
    `inst/extdata`. The changes will be shown in the Git pane in
    RStudio. Commit these files to Git, and push it to GitHub. Go to the
    GitHub repository, and open a pull request from your branch (created
    in step 1). Best practice is to have someone review the PR, but if
    you know the changes are good, you can merge it yourself. At the
    very least, ensure that all of the automated checks pass before you
    merge.

10. Once your update is merged, install the new version:
    `devtools::install_github("bcgov/wqbench")`.

### Review process

Below are instructions for how to fill out and complete each of the
reference files.

#### Concentration Conversion

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
  - Once completed, this file should be saved in the `"completed"`
    subfolder in the review folder.

If any incorrect conversions are found, then those rows can be updated.

#### Duration Conversion

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
  - Once completed, this file should be saved in the `"completed"`
    subfolder in the review folder.

If any incorrect conversions are found, then those rows can be updated.

#### Trophic Groups

This data set should be reviewed each time a new version of the ECOTOX
database is released.

- Two files will be generated to help review the trophic group data.
  - *missing-trophic-group-review.csv*
    - This is a summary that shows the unique phylum, class, order, and
      family of new taxa in the new version of Ecotox database, that do
      not meet the exclusion criteria. In other words, these are
      candidate taxa to be added to the internal dataset.
    - This file contains three columns for the reviewer to consider:
      **trophic_group**, **ecological_group**, and **exclude_from_db**.
      - If the taxon is not appropriate for inclusion, put a `1` in the
        **exclude_from_db** column. Otherwise leave it blank.
      - If it is appropriate to include, fill out the **trophic_group**
        and **ecological_group** columns. Valid values for
        **trophic_group** are: “Invertebrate”, “Algae”, “Amphibian”,
        “Plant”, “Bacteria”, “Fish”. Valid values for
        **ecological_group** are: “Planktonic Invertebrate”, “Other”,
        “Salmonid”.
      - If there is a row in the “missing-trophic-review” sheet that
        doesn’t have family or order, do one of two things: if the
        phylum/division is really small and/or the whole taxon can be
        assigned an ecological group and trophic group, do so. If not
        (e.g. Annelida, Arthropoda), put `1` in the **exclude_from_db**
        column so that high-level taxon won’t appear for you to review
        again, but lower levels within that phylum/division will. It is
        also likely that lower taxonomic entities have been assigned to
        the appropriate trophic group and ecological group.
    - Once completed, this file should be saved in the `"completed"`
      subfolder in the review folder.
  - *species-coded-in-db-ref.csv*
    - This is a list of all the species data from the database that have
      been filtered where **organism_habitat** is “Water”.
    - This file is to help find which **class** and **tax_order**
      (order) do not have coding for the ecological or trophic groups.
    - This file is for reference and not to be updated or sent back for
      integration.

#### Life Stage Codes

This data set should be reviewed each time a new version of the ECOTOX
database is released.

This data set will be sent separately after the first round of files is
reviewed because this data depends on the updates to the trophic group
data. Only fish and amphibian groups need the life stage categorized
into simple groups, so this file only contains fish and amphibians.

*lifestage-code-review.csv*: - If there is no value in the
**simple_lifestage** column this indicates it is a new life stage that
was not in the previous coding of the database. - The goal of the review
is to ensure all cells in the **simple_lifestage** are filled in. - In
the **simple_lifestage** column fill in any empty cells with either
`els` (early life stage), `juvenile` or `adult`.

Once completed, this file should be saved in the `"completed"` subfolder
in the review folder.

#### BC Species

This reference dataset has not been included in the
*review-reference-datasets.R* script as it should not vary from year to
year.

This data set is a comprehensive list that was generated from B.C.
Conservation Data Centre.

Any species that is not listed in the *bc-species.csv* reference file is
marked as not present in BC and thus it is robust to new species added
to the ECOTOX dataset.

#### Concentration Endpoints

This reference dataset has not been included in the
*review-reference-datasets.R* script as it should not vary from year to
year.

There is a comprehensive list that should not need to be adjusted.

The list of concentration endpoints are generated in the
*scripts/concentration-endpoints.R* script.

If updates are required then update the *concentration-endpoints.R*
script.

### Updating the Add Data Template

**This will rarely need to be done as additions / removals to ecological
groups and trophic groups are uncommon**

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

## Push and install the pacakge

Finally, after all of the all of the updates are completed, push them to
GitHub. Install the updated package with
`devtools::install_github("bcgov/wqbench")`.
