`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

arg_match_twin <- function(arg, values, arg_name = deparse(substitute(arg))) {
  if (length(arg) != 1L || is.na(arg) || !arg %in% values) {
    cli_abort_twin(
      "{.arg {arg_name}} must be one of {.val {values}}.",
      class = "twinfake_bad_argument"
    )
  }
  arg
}

check_bool <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    cli_abort_twin("{.arg {name}} must be `TRUE` or `FALSE`.")
  }
  invisible(x)
}

check_dir_readable <- function(path, name = "input_dir") {
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    cli_abort_twin("{.arg {name}} must be a single path string.")
  }
  if (!dir.exists(path)) {
    cli_abort_twin("{.arg {name}} does not exist: {.path {path}}.")
  }
  invisible(path)
}

safe_rel_path <- function(path, root) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  sub(paste0("^", escape_regex(root), "/?"), "", path)
}

escape_regex <- function(x) {
  gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x)
}

is_subpath <- function(child, parent) {
  child <- normalizePath(child, winslash = "/", mustWork = FALSE)
  parent <- normalizePath(parent, winslash = "/", mustWork = FALSE)
  identical(child, parent) || startsWith(paste0(child, "/"), paste0(parent, "/"))
}

require_suggested <- function(pkg, why = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    msg <- if (is.null(why)) {
      "Package {.pkg {pkg}} is required for this operation."
    } else {
      paste0("Package {.pkg {pkg}} is required ", why, ".")
    }
    cli_abort_twin(c(
      msg,
      i = "Install it to use this optional file type or feature."
    ), class = "twinfake_missing_suggested_package")
  }
  invisible(TRUE)
}

as_data_frame_preserve <- function(x) {
  if (inherits(x, "data.table") && requireNamespace("data.table", quietly = TRUE)) {
    return(data.table::as.data.table(x))
  }
  if (tibble::is_tibble(x)) {
    return(tibble::as_tibble(x))
  }
  as.data.frame(x, stringsAsFactors = FALSE)
}

safe_col_classes <- function(x) {
  stats::setNames(lapply(x, class), names(x))
}

safe_dim <- function(x) {
  d <- dim(x)
  if (is.null(d)) length(x) else unname(d)
}

vec_missing <- function(x) {
  out <- is.na(x)
  out[is.nan(out)] <- TRUE
  out
}

is_atomicish <- function(x) {
  is.atomic(x) || is.factor(x) || inherits(x, c("Date", "POSIXt", "difftime"))
}

as_safe_character <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(format(x, usetz = TRUE))
  }
  as.character(x)
}
