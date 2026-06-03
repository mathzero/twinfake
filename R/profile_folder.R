#' Profile a folder of data files
#'
#' Scans supported files and returns privacy-safe structure, schema,
#' missingness, dirty-pattern, and dependency summaries. The returned object does
#' not include raw sensitive values by default.
#'
#' @param input_dir Folder to profile.
#' @param recursive Recurse into subdirectories.
#' @param include Optional regex of relative paths to include.
#' @param exclude Optional regex of relative paths to exclude.
#' @param sensitivity Default sensitivity. `"all"` treats all columns as
#'   sensitive.
#' @param public_codes Optional columns whose labels may be preserved.
#' @param key_cols Optional key-column hints.
#' @param risk_level One of `"strict"`, `"balanced"`, or `"utility"`.
#' @return A list with class `twinfake_folder_profile`.
#' @export
profile_folder <- function(
    input_dir,
    recursive = TRUE,
    include = NULL,
    exclude = NULL,
    sensitivity = "all",
    public_codes = NULL,
    key_cols = NULL,
    risk_level = c("strict", "balanced", "utility")) {
  profile_folder_impl(
    input_dir = input_dir,
    recursive = recursive,
    include = include,
    exclude = exclude,
    sensitivity = sensitivity,
    public_codes = public_codes,
    key_cols = key_cols,
    risk_level = risk_level,
    progress = NULL
  )
}

profile_folder_impl <- function(
    input_dir,
    recursive = TRUE,
    include = NULL,
    exclude = NULL,
    sensitivity = "all",
    public_codes = NULL,
    key_cols = NULL,
    risk_level = c("strict", "balanced", "utility"),
    progress = NULL) {
  input_dir <- check_dir_readable(input_dir, "input_dir")
  check_bool(recursive, "recursive")
  risk_level <- arg_match_twin(risk_level[[1L]], c("strict", "balanced", "utility"), "risk_level")
  input_dir <- normalizePath(input_dir, winslash = "/", mustWork = TRUE)
  paths <- discover_input_files(input_dir, recursive = recursive, include = include, exclude = exclude)

  profiles <- list()
  skipped <- list()
  spec <- normalize_twin_spec(sensitivity = sensitivity, public_codes = public_codes, key_cols = key_cols, risk_level = risk_level)
  for (i in seq_along(paths)) {
    path <- paths[[i]]
    rel <- safe_rel_path(path, input_dir)
    if (is.function(progress)) {
      progress(i, length(paths), rel)
    }
    if (!is_supported_file(path)) {
      skipped[[rel]] <- list(path = rel, status = "unsupported")
      next
    }
    entry <- read_twin_file(path, rel_path = rel)
    profiles[[rel]] <- profile_entry(entry, spec = spec, risk_level = risk_level)
  }
  out <- list(
    input_dir = input_dir,
    files = profiles,
    skipped = skipped,
    risk_level = risk_level
  )
  class(out) <- c("twinfake_folder_profile", class(out))
  out
}

profile_entry <- function(entry, spec, risk_level) {
  if (entry$type == "table") {
    return(profile_object(entry$data, spec = spec, risk_level = risk_level, file_id = entry$rel_path))
  }
  if (entry$type == "excel") {
    out <- list()
    for (sheet in names(entry$data)) {
      out[[sheet]] <- profile_object(entry$data[[sheet]], spec = spec, risk_level = risk_level, file_id = entry$rel_path, sheet = sheet)
    }
    return(out)
  }
  if (entry$type == "rdata") {
    out <- list()
    for (nm in names(entry$data)) {
      out[[nm]] <- profile_object(entry$data[[nm]], spec = spec, risk_level = risk_level, file_id = entry$rel_path, sheet = nm)
    }
    return(out)
  }
  profile_object(entry$data, spec = spec, risk_level = risk_level)
}
