ableToRun <- function() {
  list(
    CDMC = all(
      require("CirceR", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE),
      require("CDMConnector", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE),
      require("DBI", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE),
      require("duckdb", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
    ),

    CG = all(
      require("CirceR", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE),
      require("CohortGenerator", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE),
      require("DatabaseConnector", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE),
      require("SqlRender", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE),
      require("Eunomia", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
    ),

    plotting = all(
      require("ggplot2", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE),
      require("webshot2", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE),
      require("plotly", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
    )
  )
}
