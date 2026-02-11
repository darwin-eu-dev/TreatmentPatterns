# getResultsDataModelSpecifications

Gets the results data model specifications of `TreatmentPatterns`.

## Usage

``` r
getResultsDataModelSpecifications()
```

## Value

`data.frame`

## Examples

``` r
{
getResultsDataModelSpecifications()
}
#>                 tableName                     columnName dataType isRequired
#> 1               attrition                 number_records      int        yes
#> 2               attrition                number_subjects      int        yes
#> 3               attrition                      reason_id      int        yes
#> 4               attrition                         reason  varchar        yes
#> 5               attrition                           time   bigint        yes
#> 6               attrition                    analysis_id      int        yes
#> 7              counts_age                            age      int        yes
#> 8              counts_age                              n  varchar        yes
#> 9              counts_age                    analysis_id      int        yes
#> 10             counts_sex                            sex  varchar        yes
#> 11             counts_sex                              n  varchar        yes
#> 12             counts_sex                    analysis_id      int        yes
#> 13            counts_year                           year      int        yes
#> 14            counts_year                              n  varchar        yes
#> 15            counts_year                    analysis_id      int        yes
#> 16               metadata                execution_start   bigint        yes
#> 17               metadata                package_version  varchar        yes
#> 18               metadata                      r_version  varchar        yes
#> 19               metadata                       platform  varchar        yes
#> 20               metadata                  execution_end   bigint        yes
#> 21               metadata                    analysis_id      int        yes
#> 22 summary_event_duration                     event_name  varchar        yes
#> 23 summary_event_duration                            min      int        yes
#> 24 summary_event_duration                             q1      int        yes
#> 25 summary_event_duration                         median      int        yes
#> 26 summary_event_duration                             q2      int        yes
#> 27 summary_event_duration                            max      int        yes
#> 28 summary_event_duration                        average    float        yes
#> 29 summary_event_duration                             sd    float        yes
#> 30 summary_event_duration                          count      int        yes
#> 31 summary_event_duration                           line  varchar        yes
#> 32 summary_event_duration                    analysis_id      int        yes
#> 33     treatment_pathways                           path  varchar        yes
#> 34     treatment_pathways                           freq      int        yes
#> 35     treatment_pathways                            age  varchar        yes
#> 36     treatment_pathways                            sex  varchar        yes
#> 37     treatment_pathways                     index_year  varchar        yes
#> 38     treatment_pathways                    analysis_id      int        yes
#> 39        cdm_source_info                cdm_source_name  varchar        yes
#> 40        cdm_source_info        cdm_source_abbreviation  varchar        yes
#> 41        cdm_source_info                     cdm_holder  varchar        yes
#> 42        cdm_source_info             source_description  varchar        yes
#> 43        cdm_source_info source_documentation_reference  varchar        yes
#> 44        cdm_source_info              cdm_etl_reference  varchar        yes
#> 45        cdm_source_info            source_release_date     date        yes
#> 46        cdm_source_info               cdm_release_date     date        yes
#> 47        cdm_source_info                    cdm_version  varchar        yes
#> 48        cdm_source_info             vocabulary_version  varchar        yes
#> 49        cdm_source_info                    analysis_id      int        yes
#> 50               analyses                    analysis_id      int        yes
#> 51               analyses                    description  varchar        yes
#> 52              arguments                    analysis_id      int        yes
#> 53              arguments                      arguments  varchar         no
#>    primaryKey
#> 1          no
#> 2          no
#> 3          no
#> 4          no
#> 5          no
#> 6          no
#> 7          no
#> 8          no
#> 9          no
#> 10         no
#> 11         no
#> 12         no
#> 13         no
#> 14         no
#> 15         no
#> 16         no
#> 17         no
#> 18         no
#> 19         no
#> 20         no
#> 21         no
#> 22         no
#> 23         no
#> 24         no
#> 25         no
#> 26         no
#> 27         no
#> 28         no
#> 29         no
#> 30         no
#> 31         no
#> 32         no
#> 33         no
#> 34         no
#> 35         no
#> 36         no
#> 37         no
#> 38         no
#> 39         no
#> 40         no
#> 41         no
#> 42         no
#> 43         no
#> 44         no
#> 45         no
#> 46         no
#> 47         no
#> 48         no
#> 49         no
#> 50         no
#> 51         no
#> 52         no
#> 53         no
#>                                                                                                                       description
#> 1                                                                                                               Number of records
#> 2                                                                                                              Number of subjects
#> 3                                                                                                               Reason Identifier
#> 4                                                                                                              Reason description
#> 5                                                                                  Time stamp in seconds since epoch (1970-01-01)
#> 6                                                                                                 Analysis identifier foreign key
#> 7                                                                                                                    Age in years
#> 8                                                                                                        Count per age. May be <x
#> 9                                                                                                 Analysis identifier foreign key
#> 10                                                                                                                      Sex group
#> 11                                                                                                       Count per sex. May be <x
#> 12                                                                                                Analysis identifier foreign key
#> 13                                                                                                                  Calendar year
#> 14                                                                                                      Count per year. May be <x
#> 15                                                                                                Analysis identifier foreign key
#> 16                                                                                 Time stamp in seconds since epoch (1970-01-01)
#> 17                                                                                                      TreatmentPatterns version
#> 18                                                                                                                      R version
#> 19                                                                                         Platform (Example: x86_64-w64-mingw32)
#> 20                                                                                 Time stamp in seconds since epoch (1970-01-01)
#> 21                                                                                                Analysis identifier foreign key
#> 22                                                                                                    Name of (combination) event
#> 23                                                                                                       Minimum duration in days
#> 24                                                                                                            Q1 duration in days
#> 25                                                                                                        Median duration in days
#> 26                                                                                                            Q2 duration in days
#> 27                                                                                                       Maximum duration in days
#> 28                                                                                                       Average duration in days
#> 29                                                                                         Standard Deviation of duration in days
#> 30                                                                                                   Count of (combination) event
#> 31 Position in pathway. I.e. 1 equals the first event in a pathway. 2 the second etc. Overall indicates across the entire pathway
#> 32                                                                                                Analysis identifier foreign key
#> 33                                                                                                                        Pathway
#> 34                                                                                                               Count of pathway
#> 35                                                                                                                    Age stratum
#> 36                                                                                                                    Sex stratum
#> 37                                                                                                      Target index year stratum
#> 38                                                                                                Analysis identifier foreign key
#> 39                                                                                                                CDM Source Name
#> 40                                                                                                        CDM Source Abbreviation
#> 41                                                                                                                     Cdm holder
#> 42                                                                                                             Source description
#> 43                                                                                                 Source Documentation Reference
#> 44                                                                                                              CDM ETL Reference
#> 45                                                                                                            Source release date
#> 46                                                                                                               CDM release date
#> 47                                                                                                                    CDM version
#> 48                                                                                                             Vocabulary version
#> 49                                                                                                Analysis identifier foreign key
#> 50                                                                                                            Analysis identifier
#> 51                                                                                                           Analysis description
#> 52                                                                                                            Analysis identifier
#> 53                                                                                                              Arguments as JSON
```
