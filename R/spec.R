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
    "default",
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

  if (!out$sensitivity %in% valid_sensitivities()) {
    cli_abort_twin(
      "Column {.field {column}} has unsupported sensitivity {.val {out$sensitivity}}.",
      class = "twinfake_bad_spec"
    )
  }
  out
}

valid_sensitivities <- function() {
  c("sensitive", "public_code", "permute", "copy", "drop", "hash", "structure_only")
}

sensitivity_choice_labels <- function() {
  c(
    "Generate fake values" = "sensitive",
    "Preserve public codes" = "public_code",
    "Permute original values" = "permute",
    "Copy original values" = "copy",
    "Blank/drop values" = "drop",
    "Hash values" = "hash",
    "Structure only" = "structure_only"
  )
}

sensitivity_action_details <- function() {
  data.frame(
    sensitivity = valid_sensitivities(),
    label = unname(names(sensitivity_choice_labels())),
    effect = c(
      paste(
        "Replace values with synthetic values of the same broad type.",
        "Preserves row count, missingness, duplicate patterns, category frequencies,",
        "and broad numeric/date distributions. Key-like columns use stable fake keys."
      ),
      paste(
        "Reuse the original non-missing labels as allowed categories and sample from them.",
        "Preserves public label sets, observed frequencies, and missingness."
      ),
      paste(
        "Shuffle the existing column values across rows when row count is unchanged.",
        "Detected child columns reuse the same shuffle unless the child action drops",
        "or structure-only blanks the relationship."
      ),
      "Copy the original column values in their original row order.",
      "Replace values with typed missing values while keeping the column in the output schema.",
      paste(
        "Replace values with salted deterministic hashes.",
        "The same input value and salt produce the same hash."
      ),
      paste(
        "Replace non-missing values with simple placeholders such as TEXT_PLACEHOLDER,",
        "LEVEL_PLACEHOLDER, 0, or FALSE while keeping type and missingness."
      )
    ),
    disclosure = c(
      "Default privacy-first option. Does not intentionally retain raw values.",
      "Use only for codes or labels that are genuinely safe to disclose.",
      "Retains real values, marginal distributions, and opted-in linked pairs. Use only after review.",
      "Retains raw values and row-level associations. Highest disclosure risk.",
      "Does not retain original values, but removes column utility apart from schema and missingness.",
      "Can leak equality and frequency patterns, especially for small categorical domains.",
      "Lowest utility option. Useful when only shape and type are needed."
    ),
    stringsAsFactors = FALSE
  )
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
