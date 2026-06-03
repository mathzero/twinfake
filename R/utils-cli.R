cli_abort_twin <- function(message, ..., class = NULL) {
  cli::cli_abort(message, ..., class = c(class, "twinfake_error"))
}

cli_warn_twin <- function(message, ..., class = NULL) {
  cli::cli_warn(message, ..., class = c(class, "twinfake_warning"))
}

cli_inform_twin <- function(message, ..., quiet = FALSE) {
  if (!isTRUE(quiet)) {
    cli::cli_inform(message, ...)
  }
}

package_version_string <- function() {
  tryCatch(
    as.character(utils::packageVersion("twinfake")),
    error = function(e) "0.0.1"
  )
}
