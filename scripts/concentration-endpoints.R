# Copyright 2024 Province of British Columbia
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at 
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

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
