# Copyright 2026 DARWIN EU®
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

initMetadata <- function(andromeda) {
  andromeda$metadata <- data.frame(
    execution_start = Sys.time(),
    package_version = as.character(utils::packageVersion("TreatmentPatterns")),
    r_version = base::version$version.string,
    platform = base::version$platform
  )
  return(invisible(andromeda))
}