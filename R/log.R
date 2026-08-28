initLog <- function(andromeda) {
  andromeda$log <- data.frame(
    time = Sys.time(),
    level = factor("INFO", levels = c("ERROR", "WARN", "INFO")),
    message = "Created log table"
  )
  return(invisible(andromeda))
}

appendLog <- function(andromeda, msg, level = "INFO") {
  log <- data.frame(
    time = Sys.time(),
    level = factor(level, levels = c("ERROR", "WARN", "INFO")),
    message = msg
  )
  
  andromeda$log <- andromeda$log |>
    dplyr::union_all(log, copy = TRUE)

  return(invisible(andromeda))
}
