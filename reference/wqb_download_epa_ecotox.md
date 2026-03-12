# Download ECOTOX Data Files

Download the ECOTOX data files from their FTP website. The default is to
download the most recent version of the data.

## Usage

``` r
wqb_download_epa_ecotox(
  file_path = ".",
  version = 1,
  ask = TRUE,
  quiet = FALSE
)
```

## Arguments

- file_path:

  A string of the file path location to save the downloaded files. The
  default is your current working directory.

- version:

  An integer to indicate which version you want to download. The default
  is 1 which downloads the most recent version.

- ask:

  Turn off question when set to FALSE.

- quiet:

  Turn off message when quiet set to TRUE.

## Value

Invisible string of the file path the downloaded files were saved.

## Details

You have the option of downloading older version of the data but only up
to the four most recent version. The most recent version is set as 1 and
the oldest version is version 4.

The downloaded folder will contain various files that are needed to
build the database.

You must have a working internet connection to run this function
successfully.

## References

US EPA ECOTOX website: <https://cfpub.epa.gov/ecotox/>

Olker, J. H., Elonen, C. M., Pilli, A., Anderson, A., Kinziger, B.,
Erickson, S., Skopinski, M., Pomplun, A., LaLone, C. A., Russom, C. L.,
& Hoff, D. (2022). The ECOTOXicology Knowledgebase: A Curated Database
of Ecologically Relevant Toxicity Tests to Support Environmental
Research and Risk Assessment. Environmental Toxicology and Chemistry,
41(6):1520-1539. https://doi.org/10.1002/etc.5324

## Examples

``` r
if (FALSE) { # \dontrun{
wqb_download_epa_ecotox()

wqb_download_epa_ecotox("data_download")

# pull previous version of the database
wqb_download_epa_ecotox("data_download", version = 2)
} # }
```
