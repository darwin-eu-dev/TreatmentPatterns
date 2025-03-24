SELECT
  @cohortTable.cohort_definition_id,
  @cohortTable.subject_id,
  @cohortTable.cohort_start_date,
  @cohortTable.cohort_end_date,
  YEAR(@cohortTable.cohort_start_date) - person.year_of_birth AS age,
  concept.concept_name AS sex,
  subject_id_origin
FROM
  @resultSchema.@cohortTable
INNER JOIN @cdmSchema.person
  ON @cohortTable.subject_id = person.person_id
INNER JOIN @cdmSchema.concept
  ON person.gender_concept_id = concept.concept_id
INNER JOIN
  (
    SELECT
      ROW_NUMBER() OVER (ORDER BY subject_id) AS subject_id,
      CAST(subject_id AS VARCHAR) AS subject_id_origin
    FROM @resultSchema.@cohortTable
  ) org_subject_table
  ON CAST(subject_id_origin AS VARCHAR) = CAST(@cohortTable.subject_id AS VARCHAR)
WHERE
  cohort_definition_id IN (@cohortIds)
  AND DATEDIFF(d, cohort_start_date, cohort_end_date) >= @minEraDuration