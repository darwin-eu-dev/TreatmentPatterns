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

fetchAttritionCounts <- function(andromeda, table = "treatmentHistory") {
  n <- andromeda[[table]] %>%
    dplyr::group_by(.data$personId) %>% 
    dplyr::summarise(n = dplyr::n()) %>%
    dplyr::pull()
  list(
    nSubjects = length(n),
    nRecords = sum(n)
  )
}

initAttrition <- function(andromeda) {
  andromeda$attrition <- data.frame(
    number_target_subjects = numeric(0),
    number_target_records = numeric(0),
    number_event_subjects = numeric(0),
    number_event_records = numeric(0),
    reason_id = numeric(0),
    reason = character(0),
    time_stamp = numeric(0)
  )
  return(andromeda)
}

appendAttrition <- function(toAdd, andromeda) {
  if (is.null(andromeda$attrition)) initAttrition(andromeda)
  
  toAdd$time_stamp <- as.numeric(Sys.time())
  
  andromeda$attrition <- dplyr::rows_append(
    x = andromeda$attrition,
    y = toAdd,
    copy = TRUE
  )

  message(sprintf(
    "-- %s\n\tTarget Subjects: %s (%s)\n\tEvents Subjects: %s (%s)",
    toAdd$reason,
    toAdd$number_target_subjects,
    toAdd$number_target_records,
    toAdd$number_event_subjects,
    toAdd$number_event_records
  ))
}
