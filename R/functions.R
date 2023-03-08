
geometric_mean <- function(x) {
  exp(mean(log(x)))
}

gen_af_table <- function(data) {
  chk::check_data(
    data,
    list(
      af_variation = 1L, 
      af_salmon = 1L, 
      af_planktonic = 1L, 
      af_bc_species = 1L
    )
  )
  
  af_table <- data |>
    dplyr::select(
      "af_variation", "af_salmon", "af_planktonic", "af_bc_species"
    ) |>
    dplyr::distinct() |>
    tidyr::pivot_longer(
      cols = c("af_variation", "af_salmon", "af_planktonic", "af_bc_species"),
      names_to = "name",
      values_to = "value"
    ) 
  af_table
}

gen_af_value <- function(data) {
  af_table <- gen_af_table(data)
  af <- prod(af_table$value)
  af
}