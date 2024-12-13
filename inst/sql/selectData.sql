SELECT
  @resultSchema.@cohortTable.cohort_definition_id,
  @resultSchema.@cohortTable.subject_id,
  @resultSchema.@cohortTable.cohort_start_date,
  @resultSchema.@cohortTable.cohort_end_date,
  YEAR(@resultSchema.@cohortTable.cohort_start_date) - @cdmSchema.person.year_of_birth AS age,
  @cdmSchema.concept.concept_name AS sex,
  subject_id_origin
FROM
  @resultSchema.@cohortTable
INNER JOIN @cdmSchema.person
  ON @resultSchema.@cohortTable.subject_id = @cdmSchema.person.person_id
INNER JOIN @cdmSchema.concept
  ON @cdmSchema.person.gender_concept_id = @cdmSchema.concept.concept_id
INNER JOIN
  (
    SELECT
      ROW_NUMBER() OVER (PARTITION BY subject_id) AS subject_id,
      CAST(subject_id AS VARCHAR) AS subject_id_origin
    FROM @resultSchema.@cohortTable
    GROUP BY subject_id
  ) org_subject_table
  ON subject_id_origin = @resultSchema.@cohortTable.subject_id
WHERE
  cohort_definition_id IN (@cohortIds)
  AND DATEDIFF(d, cohort_start_date, cohort_end_date) >= @minEraDuration