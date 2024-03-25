## code to prepare `concentration-endpoints` dataset goes here

values <- stringr::str_pad(seq(from = 5, to = 55, by = 1), 2, pad = "0")

ec <- paste0("EC", values)
ec_star <- paste0(ec, "*")

ic <- paste0("IC", values)
ic_star <- paste0(ic, "*")

lc <- paste0("LC", values)
lc_star <- paste0(lc, "*")

non_numeric <- c("LOEC", "LOEL", "MATC", "MCIG", "NOEL", "NOEC")
non_numeric_star <- paste0(non_numeric, "*")

log_codes <-  c("(log)EC50", "(log)LC50", "(log)IC50")

codes <- c(
  ec, ec_star,
  ic, ic_star,
  lc, lc_star,
  log_codes,
  non_numeric,
  non_numeric_star
)

conc_endpoints <- data.frame(code = codes)

readr::write_csv(conc_endpoints, "inst/extdata/concentration-endpoints.csv")
