profile_object <- function(x, ...) {
  UseMethod("profile_object")
}

profile_object.default <- function(x, sensitivity = "all", public_codes = NULL, key_cols = NULL, risk_level = "strict", ...) {
  out <- list(
    type = "object",
    class = class(x),
    typeof = typeof(x),
    dim = safe_dim(x),
    warning = "Unsupported object profiled by structure only."
  )
  class(out) <- c("twinfake_object_profile", class(out))
  out
}

profile_object.data.frame <- function(x, sensitivity = "all", public_codes = NULL, key_cols = NULL, risk_level = "strict", file_id = NULL, sheet = NULL, spec = NULL, ...) {
  spec <- normalize_twin_spec(
    spec = spec,
    sensitivity = sensitivity,
    public_codes = public_codes,
    key_cols = key_cols,
    risk_level = risk_level
  )
  col_profiles <- list()
  col_names <- names(x)
  for (i in seq_along(x)) {
    col <- col_names[[i]]
    control <- column_control(spec, col, file_id = file_id, sheet = sheet)
    col_profiles[[i]] <- profile_vec(
      x[[i]],
      sensitivity = control$sensitivity,
      public_code = control$sensitivity == "public_code",
      risk_level = risk_level
    )
  }
  names(col_profiles) <- col_names
  out <- list(
    type = "data.frame",
    class = class(x),
    nrow = nrow(x),
    ncol = ncol(x),
    column_names = names(x),
    column_classes = safe_col_classes(x),
    columns = col_profiles,
    dependencies = detect_dependencies(x),
    key_suggestions = names(x)[vapply(names(x), key_like_name, logical(1L))],
    risk_level = risk_level
  )
  class(out) <- c("twinfake_profile", "twinfake_object_profile", class(out))
  out
}

profile_object.list <- function(x, sensitivity = "all", public_codes = NULL, key_cols = NULL, risk_level = "strict", ...) {
  out <- list(
    type = "list",
    class = class(x),
    names = names(x),
    length = length(x),
    elements = lapply(x, profile_object, sensitivity = sensitivity, public_codes = public_codes, key_cols = key_cols, risk_level = risk_level)
  )
  class(out) <- c("twinfake_object_profile", class(out))
  out
}

#' Profile a data object for pipeline twin generation
#'
#' Profiles structure, classes, missingness, dirty patterns, broad distributions,
#' and simple deterministic relationships without storing raw sensitive values
#' by default. Column names are retained because they are needed for pipeline
#' compatibility; users should review whether names themselves are sensitive.
#'
#' @param x A data frame or object to profile.
#' @param sensitivity Default sensitivity. `"all"` treats all columns as
#'   sensitive.
#' @param public_codes Optional character vector of columns whose labels may be
#'   preserved in the profile and fake data.
#' @param key_cols Optional manual key-column specification.
#' @param deterministic Optional deterministic relationship hints. Reserved for
#'   future versions.
#' @param risk_level One of `"strict"`, `"balanced"`, or `"utility"`.
#' @return A `twinfake_profile` object containing privacy-safe metadata.
#' @export
profile_data <- function(
    x,
    sensitivity = "all",
    public_codes = NULL,
    key_cols = NULL,
    deterministic = NULL,
    risk_level = c("strict", "balanced", "utility")) {
  risk_level <- arg_match_twin(risk_level[[1L]], c("strict", "balanced", "utility"), "risk_level")
  profile_object(
    x,
    sensitivity = sensitivity,
    public_codes = public_codes,
    key_cols = key_cols,
    risk_level = risk_level,
    deterministic = deterministic
  )
}

print.twinfake_profile <- function(x, ...) {
  cli::cli_text("twinfake profile: {x$type}, {x$nrow %||% x$length} rows/items, {x$ncol %||% length(x$elements)} columns/elements")
  if (!is.null(x$key_suggestions) && length(x$key_suggestions)) {
    cli::cli_text("Key-like columns: {paste(x$key_suggestions, collapse = ', ')}")
  }
  invisible(x)
}
