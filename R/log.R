initLog <- function(andromeda) {
  andromeda$log <- data.frame(
    time = Sys.time(),
    message = "Created log table"
  )
  return(invisible(andromeda))
}

appendLog <- function(andromeda, msg) {
  log <- data.frame(
    time = Sys.time(),
    message = msg
  )
  
  andromeda$log <- andromeda$log |>
    dplyr::union_all(log, copy = TRUE)
  
  return(invisible(andromeda))
}
