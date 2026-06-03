#' Create a fake mirrored folder tree
#'
#' Recursively scans supported files in `input_dir`, generates privacy-first fake
#' equivalents, and writes a parallel folder tree under `output_dir`. Relative
#' paths, file names, column names, sheet names, row counts, missingness, dirty
#' patterns, deterministic relationships, and cross-file key behaviour are
#' preserved where supported. Raw real values and real-to-fake key maps are kept
#' in memory only and are never written to the manifest.
#'
#' @param input_dir Folder containing private source files.
#' @param output_dir Folder to create fake files in.
#' @param spec Optional spec list or path to a spec JSON file.
#' @param seed Optional numeric seed.
#' @param overwrite Overwrite existing output files or a non-empty output folder.
#' @param recursive Recurse into subdirectories.
#' @param include Optional regex of relative paths to include.
#' @param exclude Optional regex of relative paths to exclude.
#' @param sensitivity Default sensitivity; `"all"` treats all columns as
#'   sensitive.
#' @param preserve_file_names Preserve original relative file names.
#' @param preserve_row_count Preserve row counts.
#' @param xlsx_sheets `"all"` or a character vector of sheet names.
#' @param unknown How to handle unknown files: `"skip"`, `"empty_placeholder"`,
#'   or dangerous opt-in `"copy"`.
#' @param engine One of `"pipeline"`, `"synthpop"`, or `"independent"`.
#' @param risk_level One of `"strict"`, `"balanced"`, or `"utility"`.
#' @param report Write `twinfake_manifest.json`.
#' @param quiet Suppress informational messages.
#' @return A `twinfake_folder_result` object.
#' @export
make_fake_folder <- function(
    input_dir,
    output_dir,
    spec = NULL,
    seed = NULL,
    overwrite = FALSE,
    recursive = TRUE,
    include = NULL,
    exclude = NULL,
    sensitivity = "all",
    preserve_file_names = TRUE,
    preserve_row_count = TRUE,
    xlsx_sheets = "all",
    unknown = c("skip", "empty_placeholder", "copy"),
    engine = c("pipeline", "synthpop", "independent"),
    risk_level = c("strict", "balanced", "utility"),
    report = TRUE,
    quiet = FALSE) {
  input_dir <- check_dir_readable(input_dir, "input_dir")
  check_bool(overwrite, "overwrite")
  check_bool(recursive, "recursive")
  check_bool(preserve_file_names, "preserve_file_names")
  check_bool(preserve_row_count, "preserve_row_count")
  check_bool(report, "report")
  check_bool(quiet, "quiet")
  unknown <- arg_match_twin(unknown[[1L]], c("skip", "empty_placeholder", "copy"), "unknown")
  engine <- arg_match_twin(engine[[1L]], c("pipeline", "synthpop", "independent"), "engine")
  risk_level <- arg_match_twin(risk_level[[1L]], c("strict", "balanced", "utility"), "risk_level")
  check_engine_available(engine)

  if (is.character(spec) && length(spec) == 1L && file.exists(spec)) {
    spec <- read_twin_spec(spec)
  }
  spec <- normalize_twin_spec(spec, sensitivity = sensitivity, engine = engine, risk_level = risk_level)

  input_dir <- normalizePath(input_dir, winslash = "/", mustWork = TRUE)
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  if (is_subpath(output_dir, input_dir)) {
    cli_abort_twin(c(
      "Refusing to write fake data inside {.arg input_dir}.",
      i = "Choose an output directory outside the private source tree."
    ))
  }
  if (dir.exists(output_dir) && !overwrite && length(list.files(output_dir, all.files = TRUE, no.. = TRUE))) {
    cli_abort_twin("Refusing to write into non-empty {.arg output_dir} unless {.code overwrite = TRUE}.")
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  paths <- discover_input_files(input_dir, recursive = recursive, include = include, exclude = exclude)
  cli_inform_twin("Found {length(paths)} file{?s} to inspect.", quiet = quiet)

  entries <- list()
  manifest_entries <- list()
  for (path in paths) {
    rel <- safe_rel_path(path, input_dir)
    if (!is_supported_file(path)) {
      manifest_entries[[length(manifest_entries) + 1L]] <- handle_unknown_file(
        path = path,
        rel = rel,
        output_dir = output_dir,
        unknown = unknown,
        overwrite = overwrite
      )
      next
    }
    entries[[length(entries) + 1L]] <- read_twin_file(path, rel_path = rel, xlsx_sheets = xlsx_sheets)
  }

  key_maps <- build_key_maps(entries, spec = spec)

  generated <- with_twin_seed(seed, {
    out <- list()
    for (entry in entries) {
      output_path <- output_path_for_entry(entry, output_dir, preserve_file_names = preserve_file_names)
      fake_data <- fake_entry_data(
        entry,
        spec = spec,
        key_maps = key_maps,
        preserve_row_count = preserve_row_count,
        engine = engine,
        risk_level = risk_level,
        quiet = quiet
      )
      write_twin_file(entry, fake_data, output_path, overwrite = overwrite)
      out[[length(out) + 1L]] <- safe_manifest_for_entry(
        entry,
        output_path = output_path,
        output_dir = output_dir,
        fake_data = fake_data
      )
    }
    out
  })

  manifest_entries <- c(manifest_entries, generated)
  manifest_path <- NULL
  if (report) {
    manifest_path <- write_manifest(
      entries = manifest_entries,
      output_dir = output_dir,
      seed = seed,
      engine = engine,
      risk_level = risk_level
    )
  }

  result <- list(
    input_dir = input_dir,
    output_dir = output_dir,
    files = manifest_entries,
    manifest_path = manifest_path,
    seed = seed,
    engine = engine,
    risk_level = risk_level
  )
  class(result) <- c("twinfake_folder_result", class(result))
  result
}

discover_input_files <- function(input_dir, recursive = TRUE, include = NULL, exclude = NULL) {
  paths <- list.files(
    input_dir,
    recursive = recursive,
    full.names = TRUE,
    all.files = FALSE,
    no.. = TRUE
  )
  paths <- paths[file.info(paths)$isdir %in% FALSE]
  paths <- sort(normalizePath(paths, winslash = "/", mustWork = TRUE))
  rel <- vapply(paths, safe_rel_path, character(1L), root = input_dir)
  if (!is.null(include)) {
    keep <- grepl(include, rel)
    paths <- paths[keep]
    rel <- rel[keep]
  }
  if (!is.null(exclude)) {
    keep <- !grepl(exclude, rel)
    paths <- paths[keep]
  }
  paths
}

handle_unknown_file <- function(path, rel, output_dir, unknown, overwrite) {
  entry <- list(
    source_path = path,
    rel_path = rel,
    format = file_format(path),
    type = "unknown",
    data = NULL,
    warnings = character()
  )
  output_path <- file.path(output_dir, rel)
  if (unknown == "skip") {
    return(safe_manifest_for_entry(
      entry,
      output_path = NA_character_,
      output_dir = output_dir,
      fake_data = NULL,
      status = "skipped",
      warnings = "Unsupported file skipped."
    ))
  }
  if (unknown == "empty_placeholder") {
    if (file.exists(output_path) && !overwrite) {
      cli_abort_twin("Refusing to overwrite existing placeholder target: {.path {output_path}}.")
    }
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    file.create(output_path)
    return(safe_manifest_for_entry(
      entry,
      output_path = output_path,
      output_dir = output_dir,
      fake_data = NULL,
      status = "empty_placeholder",
      warnings = "Unsupported file replaced with an empty placeholder."
    ))
  }
  if (unknown == "copy") {
    if (file.exists(output_path) && !overwrite) {
      cli_abort_twin("Refusing to overwrite existing copy target: {.path {output_path}}.")
    }
    dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
    file.copy(path, output_path, overwrite = TRUE)
    return(safe_manifest_for_entry(
      entry,
      output_path = output_path,
      output_dir = output_dir,
      fake_data = NULL,
      status = "copied_dangerous",
      warnings = "Original unsupported file copied because unknown = 'copy' was explicitly requested."
    ))
  }
  cli_abort_twin("Unsupported {.arg unknown} value.")
}

output_path_for_entry <- function(entry, output_dir, preserve_file_names = TRUE) {
  if (isTRUE(preserve_file_names)) {
    return(file.path(output_dir, entry$rel_path))
  }
  ext <- if (nzchar(entry$format)) paste0(".", entry$format) else ""
  file.path(output_dir, paste0(safe_label("file", seq_along(entry$rel_path)), ext))
}

print.twinfake_folder_result <- function(x, ...) {
  generated <- sum(vapply(x$files, function(f) identical(f$status, "generated"), logical(1L)))
  cli::cli_text("twinfake folder result: {generated} generated file{?s} in {.path {x$output_dir}}")
  if (!is.null(x$manifest_path)) {
    cli::cli_text("Manifest: {.path {x$manifest_path}}")
  }
  invisible(x)
}
