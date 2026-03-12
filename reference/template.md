# Template for Adding your own Data

Template for Adding your own Data

## Usage

``` r
template
```

## Format

A data.frame with columns:

- name:

  Row description.

- latin_name:

  The latin name of the test species.

- endpoint:

  Toxicity endpoint.

- effect:

  The effect that was being tested.

- lifestage:

  The lifestage the species was during the test.

- effect_conc_mg.L:

  Contaminant concentration that corresponds to the endpoint.

- effect_conc_std_mg.L:

  The effect concentration standardized to include the acute to chronic
  ratio to extrapolate acute and/or effect concentrations to chronic
  and/or no-effect concentrations in mg/L.

- trophic_group:

  Trophic group of species.

- ecological_group:

  Identification of salmonids and planktonic invertebrates. If neither
  of these, listed as “other”.

- species_present_in_bc:

  Species is present in British Columbia if entry = TRUE
