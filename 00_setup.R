# Copyright 2022 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

# Setup renv ---------------------------
# - for reproducibility

# renv::init()     # - Only needs be done to get things started, no longer necessary
# renv::update()   # - Update renv packages - best done at the start of an analysis update
# renv::snapshot() # - As needed to keep renv packages up-to-date

renv::restore()   # - When updating from GitHub etc. restore to packages in lockfile

# Setup ---------------------------------
dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)
dir.create("data/datasets", showWarnings = FALSE, recursive = TRUE)
dir.create("out", showWarnings = FALSE, recursive = TRUE)
