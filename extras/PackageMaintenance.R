# Copyright 2026 Observational Health Data Sciences and Informatics
#
# This file is part of TreatmentPatterns
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Package version -------------------------------------------------------------
file.edit("DESCRIPTION")
file.edit("NEWS.md")
file.edit("cran-comments.md")

# Format and check code -------------------------------------------------------
styler::style_pkg()
OhdsiRTools::checkUsagePackage("TreatmentPatterns")
OhdsiRTools::updateCopyrightYearFolder()
devtools::spell_check()

# Create manual and vignettes -------------------------------------------------
unlink("extras/TreatmentPatterns.pdf")
shell("R CMD Rd2pdf ./ --output=extras/TreatmentPatterns.pdf")

# Release package -------------------------------------------------------------
devtools::check_win_devel()
devtools::check_win_release()
devtools::check_win_oldrelease()

devtools::check_rhub(
  platforms = c(
    "debian-clang-devel",
    "debian-gcc-devel",
    "fedora-clang-devel",
    "fedora-gcc-devel",
    "debian-gcc-patched",
    "debian-gcc-release"
  ),
  interactive = FALSE
)

# args = "--compact-vignettes=gs+qpdf"
devtools::release()
