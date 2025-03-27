SELECT
    @cohortTable.cohort_definition_id,
    new_id AS subject_id,
    @cohortTable.cohort_start_date,
    @cohortTable.cohort_end_date,
    CAST(subject_id_origin AS VARCHAR) AS subject_id_origin,
    YEAR(@cohortTable.cohort_start_date) - person.year_of_birth AS age,
    concept.concept_name AS sex
  FROM @resultSchema.@cohortTable
  INNER JOIN (
    SELECT
      ROW_NUMBER() OVER (ORDER BY subject_id) AS new_id,
      subject_id AS subject_id_origin
    FROM (
      SELECT
  	    DISTINCT @cohortTable.subject_id
      FROM
        @resultSchema.@cohortTable
    ) unique_subjects
  ) new_subject_ids
    ON @cohortTable.subject_id = subject_id_origin
  INNER JOIN @cdmSchema.person
    ON subject_id_origin = person.person_id
  INNER JOIN @cdmSchema.concept
    ON person.gender_concept_id = concept.concept_id
  WHERE
    @cohortTable.cohort_definition_id IN (@cohortIds)
    AND DATEDIFF(d, cohort_start_date, cohort_end_date) >= @minEraDuration