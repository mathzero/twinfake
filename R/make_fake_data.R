#' Create a privacy-preserving fake version of a data object
#'
#' `make_fake_data()` creates fake data that preserve column names, order,
#' classes, row count, missingness, dirty-value patterns, duplicate structure,
#' and simple deterministic relationships. All columns are treated as sensitive
#' by default. The result is for private code development and LLM-assisted
#' pipeline construction, not public release.
#'
#' @param x A data frame or supported R object.
#' @param spec Optional twinfake spec list.
#' @param seed Optional numeric seed.
#' @param sensitivity Default sensitivity. `"all"` means all columns sensitive.
#' @param preserve_row_count Preserve the input row count.
#' @param engine One of `"pipeline"`, `"synthpop"`, or `"independent"`.
#' @param risk_level One of `"strict"`, `"balanced"`, or `"utility"`.
#' @param quiet Suppress informational messages.
#' @return A fake object matching the broad structure of `x`.
#' @export
make_fake_data <- function(
    x,
    spec = NULL,
    seed = NULL,
    sensitivity = "all",
    preserve_row_count = TRUE,
    engine = c("pipeline", "synthpop", "independent"),
    risk_level = c("strict", "balanced", "utility"),
    quiet = FALSE) {
  engine <- arg_match_twin(engine[[1L]], c("pipeline", "synthpop", "independent"), "engine")
  risk_level <- arg_match_twin(risk_level[[1L]], c("strict", "balanced", "utility"), "risk_level")
  check_bool(preserve_row_count, "preserve_row_count")
  check_bool(quiet, "quiet")
  check_engine_available(engine)

  spec <- normalize_twin_spec(spec, sensitivity = sensitivity, engine = engine, risk_level = risk_level)

  with_twin_seed(seed, {
    fake_object_safe(
      x,
      spec = spec,
      preserve_row_count = preserve_row_count,
      engine = engine,
      risk_level = risk_level,
      quiet = quiet
    )
  })
}

check_engine_available <- function(engine) {
  if (engine == "synthpop") {
    require_suggested("synthpop", "to use `engine = \"synthpop\"`")
  }
  invisible(TRUE)
}

fake_data_frame <- function(
    x,
    spec = NULL,
    file_id = NULL,
    sheet = NULL,
    preserve_row_count = TRUE,
    engine = "pipeline",
    risk_level = "strict",
    key_maps = NULL,
    quiet = FALSE,
    ...) {
  if (!is.data.frame(x)) {
    cli_abort_twin("{.arg x} must be a data frame for this method.")
  }
  n <- if (preserve_row_count) nrow(x) else varied_row_count(nrow(x))
  spec <- normalize_twin_spec(spec, engine = engine, risk_level = risk_level)
  deps <- detect_dependencies(x)
  col_names <- names(x)
  controls <- vector("list", length(x))
  for (i in seq_along(x)) {
    controls[[i]] <- column_control(spec, col_names[[i]], file_id = file_id, sheet = sheet)
  }
  names(controls) <- col_names

  fake_cols <- vector("list", length(x))
  for (i in seq_along(x)) {
    col <- col_names[[i]]
    fake_cols[[i]] <- fake_vec(
      x[[i]],
      control = controls[[i]],
      name = safe_profile_column_name(col, i),
      preserve_row_count = preserve_row_count,
      n = n,
      key_map = key_map_for_column(key_maps, col),
      risk_level = risk_level
    )
  }
  names(fake_cols) <- col_names

  if (engine %in% c("pipeline", "independent") && length(deps) && n == nrow(x)) {
    for (dep in deps) {
      child_control <- controls[[dep$child]]
      if (!child_control$sensitivity %in% c("copy", "public_code", "hash", "drop")) {
        fake_cols[[dep$child]] <- apply_dependency(dep, fake_cols, x, child_control)
      }
    }
  }

  out <- new_data_frame_from_cols(fake_cols, n)
  restore_data_frame_class(out, x)
}

varied_row_count <- function(n) {
  if (n <= 0L) return(n)
  max(0L, as.integer(round(stats::rnorm(1L, mean = n, sd = max(1, sqrt(n))))))
}

new_data_frame_from_cols <- function(cols, n) {
  if (!length(cols)) {
    out <- data.frame(.row_id = seq_len(n))
    out$.row_id <- NULL
    return(out)
  }
  out <- as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE, optional = TRUE)
  names(out) <- names(cols)
  out
}

restore_data_frame_class <- function(out, template) {
  if (inherits(template, "data.table") && requireNamespace("data.table", quietly = TRUE)) {
    return(data.table::as.data.table(out))
  }
  if (tibble::is_tibble(template)) {
    return(tibble::as_tibble(out))
  }
  out
}
