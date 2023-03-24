# Copyright 2023 Province of British Columbia
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

#' Plot Species Sensitivity Distribution
#' 
#' Plot results to see the species sensitivity distribution. The dashed line
#' shows the HC5 value. 
#'
#' @param data A data frame
#' @param fit The fit from ssd
#' @export
#' @details This is a wrapper on `ssdtools::predict()` and 
#' `ssdtools::ssd_plot()`.
#'
#' @examples
#' \dontrun{
#' wqb_plot_ssd(data, fit)
#' }
wqb_plot_ssd <- function(data, fit) {
  
  pred <- ssdtools::predict(fit, ci = TRUE)
  gp <- ssdtools::ssd_plot(
    data, pred, shape = "ecological_group", color = "trophic_group", 
    label = "latin_name", left = "sp_aggre_conc_mg.L",
    xlab = "Concentration (mg/L)", ribbon = TRUE
  ) + 
    ggplot2::expand_limits(x = 3000) +
    ssdtools::scale_colour_ssd() +
    ggplot2::labs(color = "Trophic Group", shape = "Ecological Group")
  
  gp
}
