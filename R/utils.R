checkInstall <- function(pkg) {
  if (!require(pkg, quietly = TRUE, warn.conflicts = FALSE, character.only = TRUE)) {
    stop(sprintf("`%s` in an optional package, please install it with: `install.packages('%s')`", pkg, pkg))
  }
}
