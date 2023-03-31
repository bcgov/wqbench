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

skip_if_testing_quick <- function() {
  if (is.null(getOption("wqb_testing"))) {
    invisible(return())
  }
  
  if (getOption("wqb_testing") == "quick") {
    testthat::skip("Not run: testing mode set to quick")
  }
}

# To skip slower tests while developing set option to quick
options(wqb_testing = "quick")
# Comment out/restart R

