#' Write a twinfake sensitivity specification
#'
#' Writes a JSON-serialisable twinfake specification to disk. Specifications
#' should contain column roles and sensitivity classes, not real data values.
#'
#' @param spec A list describing defaults, files, columns, keys, and controls.
#' @param path Output JSON path.
#' @return Invisibly returns `path`.
#' @export
write_twin_spec <- function(spec, path) {
  if (!is.list(spec)) {
    cli_abort_twin("{.arg spec} must be a list.")
  }
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    cli_abort_twin("{.arg path} must be a single path string.")
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(spec, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  invisible(path)
}

#' Read a twinfake sensitivity specification
#'
#' Reads a JSON specification created by [write_twin_spec()] or by the local
#' Shiny app.
#'
#' @param path Path to a JSON specification.
#' @return A list with class `twinfake_spec`.
#' @export
read_twin_spec <- function(path) {
  if (!file.exists(path)) {
    cli_abort_twin("Spec file does not exist: {.path {path}}.")
  }
  out <- jsonlite::read_json(path, simplifyVector = FALSE)
  class(out) <- unique(c("twinfake_spec", class(out)))
  out
}

normalize_twin_spec <- function(
    spec = NULL,
    sensitivity = "all",
    engine = "pipeline",
    risk_level = "strict",
    public_codes = NULL,
    key_cols = NULL) {
  if (is.null(spec)) {
    spec <- list()
  }
  if (!is.list(spec)) {
    cli_abort_twin("{.arg spec} must be `NULL` or a list.")
  }

  defaults <- spec$defaults %||% list()
  defaults$sensitivity <- defaults$sensitivity %||% sensitivity_to_default(sensitivity)
  defaults$engine <- defaults$engine %||% engine
  defaults$risk_level <- defaults$risk_level %||% risk_level
  spec$defaults <- defaults

  if (!is.null(public_codes)) {
    spec$public_codes <- unique(c(spec$public_codes %||% character(), public_codes))
  }
  if (!is.null(key_cols)) {
    spec$keys <- spec$keys %||% list()
    if (is.list(key_cols)) {
      for (nm in names(key_cols)) {
        spec$keys[[nm]] <- list(columns = as.character(key_cols[[nm]]))
      }
    } else {
      spec$keys$user_keys <- list(columns = as.character(key_cols))
    }
  }
  class(spec) <- unique(c("twinfake_spec", class(spec)))
  spec
}

sensitivity_to_default <- function(sensitivity) {
  if (is.null(sensitivity) || identical(sensitivity, "all")) {
    return("sensitive")
  }
  if (is.character(sensitivity) && length(sensitivity) == 1L) {
    return(sensitivity)
  }
  "sensitive"
}

column_control <- function(spec, column, file_id = NULL, sheet = NULL) {
  spec <- normalize_twin_spec(spec)
  out <- list(
    sensitivity = spec$defaults$sensitivity %||% "sensitive",
    role = NULL,
    derived_from = NULL,
    salt = spec$defaults$salt %||% "twinfake"
  )

  if (!is.null(spec$public_codes) && column %in% spec$public_codes) {
    out$sensitivity <- "public_code"
  }

  candidates <- unique(c(
    file_id,
    if (!is.null(file_id) && !is.null(sheet)) paste0(file_id, ":", sheet),
    if (!is.null(file_id)) basename(file_id),
    if (!is.null(file_id) && !is.null(sheet)) paste0(basename(file_id), ":", sheet)
  ))
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]

  for (candidate in candidates) {
    file_spec <- spec$files[[candidate]]
    if (!is.null(file_spec) && !is.null(file_spec$columns[[column]])) {
      out <- utils::modifyList(out, file_spec$columns[[column]])
    }
  }

  valid <- c("sensitive", "public_code", "copy", "drop", "hash", "structure_only")
  if (!out$sensitivity %in% valid) {
    cli_abort_twin(
      "Column {.field {column}} has unsupported sensitivity {.val {out$sensitivity}}.",
      class = "twinfake_bad_spec"
    )
  }
  out
}

column_ref <- function(file_id, column, sheet = NULL) {
  parts <- c(file_id, sheet, column)
  paste(parts[!is.na(parts) & nzchar(parts)], collapse = ":")
}

spec_key_columns <- function(spec) {
  if (is.null(spec) || is.null(spec$keys)) {
    return(list())
  }
  out <- list()
  for (nm in names(spec$keys)) {
    cols <- spec$keys[[nm]]$columns %||% spec$keys[[nm]]
    out[[nm]] <- as.character(cols)
  }
  out
}
