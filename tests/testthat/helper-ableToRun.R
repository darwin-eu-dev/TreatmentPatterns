isOnline <- function(site="http://example.com/") {
  tryCatch({
    readLines(site,n=1)
    TRUE
  },
  warning = function(w) invokeRestart("muffleWarning"),
  error = function(e) FALSE)
}

ableToRun <- function() {
  list(
    CDMC = all(
      require("CirceR", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE),
      require("CDMConnector", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE),
      require("DBI", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE),
      require("duckdb", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE),
      isOnline()
    ),

    plotting = all(
      require("ggplot2", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE),
      require("webshot2", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE),
      require("plotly", character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
    )
  )
}
