#' Template for Adding your own Data
#'
#' @format A data.frame with columns:
#' \describe{
#'   \item{name}{Row description.}
#'   \item{latin_name}{The latin name of the test species.}
#'   \item{endpoint}{Toxicity endpoint.}
#'   \item{effect}{The effect that was being tested.}
#'   \item{lifestage}{The lifestage the species was during the test.}
#'   \item{effect_conc_std_mg.L}{The effect concentration standardized to
#'   include the acute to chronic ratio to extrapolate acute and/or effect
#'   concentrations to chronic and/or no-effect concentrations in mg/L.}
#'   \item{trophic_group}{Trophic group of species.}
#'   \item{ecological_group}{Identification of salmonids and planktonic
#'   invertebrates. If neither of these, listed as “other”.}
#'   \item{species_present_in_bc}{Species is present in British Columbia if
#'   entry = TRUE}
#' }
"template"
