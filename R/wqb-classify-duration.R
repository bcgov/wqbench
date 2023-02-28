#' Classify Duration
#'
#' Each test will be classified as an acute or chronic and a new column called 
#' `duration_class` will be added to the data set. 
#'
#' @param data A data frame of the compiled data set.
#' @return Invisible data frame
#' @export
#' @details Classifications for acute and chronic are separated by trophic group
#'   (`ecological_group`). The values are classified as acute, chronic in a new 
#'   column called `duration_class`.
#'
#'   Values not meeting this criteria are classified as not specified and are 
#'   removed.
#'  
#'  Fish and amphibians
#'    Acute: 
#'      - duration ≤ 96 hours
#'    Chronic:
#'      - life stage (`simple_lifestage`) is juvenile or adult and 
#'      duration ≥ 504 hours
#'      - life stage (`simple_lifestage`) is early life stages (els) and 
#'      duration ≥ 168 hours
#'  
#'  Aquatic invertebrates
#'    Acute: 
#'      - duration ≤ 96 hours
#'    Chronic:
#'      - Planktonic invertebrates (`ecological_group_class`) and duration > 96 hours
#'      - Regular invertebrates (`ecological_group_class`) and duration ≥ 168 hours
#'  
#'  Algae
#'    Acute: 
#'     - duration ≤ 24 hours
#'    Chronic: 
#'     - duration > 24 hours
#'    
#'  Aquatic Plants
#'    Acute: 
#'     - duration ≤ 48 hours 
#'    Chronic: 
#'    - duration > 7 days (used Lemna protocol as a guide, EC 2007)
#' 
#' Reference
#'  Environment Canada 2007. Test for measuring the inhibition of growth using 
#'  the freshwater macrophyte, Lemna minor.  Available online at: 
#'  https://publications.gc.ca/collections/collection_2013/ec/En49-7-1-37-eng.pdf
#'
#' @examples
#' \dontrun{
#' classified_data <- wqb_classify_duration(compiled_data)
#' }
wqb_classify_duration <- function(data) {
  chk::check_data(
    data, 
    list(
      ecological_group = "",
      obs_duration_mean_std = 1,
      simple_lifestage = c("", NA)
    )
  ) 
  
  data_classified <- data |>
    dplyr::mutate(
      duration_class = dplyr::case_when(
        # Fish and Amphibians
        stringr::str_detect(ecological_group, "(?i)^amphibian$|^fish$")  & obs_duration_mean_std <= 96 ~ "acute",
        stringr::str_detect(ecological_group, "(?i)^amphibian$|^fish$") & stringr::str_detect(simple_lifestage, "(?i)^juvenile$|(?i)^adult$")  & obs_duration_mean_std >= 504 ~ "chronic",
        stringr::str_detect(ecological_group, "(?i)^amphibian$|^fish$") & stringr::str_detect(simple_lifestage, "(?i)^els$")  & obs_duration_mean_std >= 168 ~ "chronic",
        # Invertebrates
        stringr::str_detect(ecological_group, "(?i)^invertebrate$") & obs_duration_mean_std <= 96 ~ "acute",
        stringr::str_detect(ecological_group, "(?i)^invertebrate$") & stringr::str_detect(ecological_group_class, "(?i)Planktonic Invertebrate") & obs_duration_mean_std > 96 ~ "chronic",
        stringr::str_detect(ecological_group, "(?i)^invertebrate$") & stringr::str_detect(ecological_group_class, "(?i)Regular") & obs_duration_mean_std >= 168 ~ "chronic",
        # Algae
        stringr::str_detect(ecological_group, "(?i)^algae$") & obs_duration_mean_std <= 24 ~ "acute",
        stringr::str_detect(ecological_group, "(?i)^algae$") & obs_duration_mean_std > 24 ~ "chronic",
        # Plants
        stringr::str_detect(ecological_group, "(?i)^plant$") & obs_duration_mean_std <= 48 ~ "acute",
        stringr::str_detect(ecological_group, "(?i)^plant$") & obs_duration_mean_std > 168 ~ "chronic",
        TRUE ~ "not specified"
      ) 
    ) |>
    dplyr::filter(!(.data$duration_class == "not specified"))
  
  data_classified
}