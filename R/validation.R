#' Validate fake data against real data structure
#'
#' Compares real and fake data for schema compatibility, class preservation,
#' missingness, dirty-pattern similarity, broad numeric and categorical
#' distribution similarity, deterministic relationship preservation, and obvious
#' privacy issues. The result does not print sensitive values.
#'
#' @param real Original data frame.
#' @param fake Fake data frame.
#' @param spec Optional twinfake spec.
#' @return A `twinfake_validation` object.
#' @export
validate_fake_data <- function(real, fake, spec = NULL) {
  if (!is.data.frame(real) || !is.data.frame(fake)) {
    cli_abort_twin("{.arg real} and {.arg fake} must both be data frames.")
  }
  common <- intersect(names(real), names(fake))
  class_match <- stats::setNames(
    vapply(common, function(col) identical(class(real[[col]]), class(fake[[col]])), logical(1L)),
    common
  )
  missingness <- stats::setNames(
    vapply(common, function(col) 1 - abs(mean(is.na(real[[col]])) - mean(is.na(fake[[col]]))), numeric(1L)),
    common
  )
  dirty <- stats::setNames(
    vapply(common, function(col) dirty_pattern_similarity(real[[col]], fake[[col]]), numeric(1L)),
    common
  )
  numeric_sim <- stats::setNames(
    vapply(common, function(col) numeric_distribution_similarity(real[[col]], fake[[col]]), numeric(1L)),
    common
  )
  categorical_sim <- stats::setNames(
    vapply(common, function(col) categorical_frequency_similarity(real[[col]], fake[[col]]), numeric(1L)),
    common
  )
  real_deps <- detect_dependencies(real)
  fake_deps <- detect_dependencies(fake)
  dep_sim <- dependency_similarity(real_deps, fake_deps)
  possible_privacy <- in_memory_privacy_issues(real, fake)

  out <- list(
    schema_match = identical(names(real), names(fake)) && length(class_match) == ncol(real),
    column_names_match = setequal(names(real), names(fake)),
    column_order_match = identical(names(real), names(fake)),
    class_match = class_match,
    row_count_match = nrow(real) == nrow(fake),
    missingness_similarity = missingness,
    dirty_pattern_similarity = dirty,
    numeric_distribution_similarity = numeric_sim,
    categorical_frequency_similarity = categorical_sim,
    dependency_similarity = dep_sim,
    key_integrity = key_integrity_similarity(real, fake),
    deterministic_relationship_preservation = dep_sim,
    possible_privacy_issues = possible_privacy,
    spec_used = !is.null(spec)
  )
  class(out) <- c("twinfake_validation", class(out))
  out
}

numeric_distribution_similarity <- function(real, fake) {
  if (!(is.numeric(real) || inherits(real, c("Date", "POSIXt", "difftime")))) {
    return(NA_real_)
  }
  r <- as.numeric(real)
  f <- as.numeric(fake)
  r <- r[is.finite(r)]
  f <- f[is.finite(f)]
  if (!length(r) && !length(f)) return(1)
  if (!length(r) || !length(f)) return(0)
  probs <- seq(0.1, 0.9, by = 0.1)
  rq <- stats::quantile(r, probs = probs, names = FALSE, na.rm = TRUE)
  fq <- stats::quantile(f, probs = probs, names = FALSE, na.rm = TRUE)
  scale <- max(stats::median(abs(r - stats::median(r))), 1)
  max(0, 1 - mean(abs(rq - fq)) / scale)
}

categorical_frequency_similarity <- function(real, fake) {
  if (!(is.character(real) || is.factor(real) || is.logical(real))) {
    return(NA_real_)
  }
  r <- sort(as.numeric(table(as_safe_character(real), useNA = "ifany")), decreasing = TRUE)
  f <- sort(as.numeric(table(as_safe_character(fake), useNA = "ifany")), decreasing = TRUE)
  n <- max(length(r), length(f))
  r <- c(r, rep(0, n - length(r)))
  f <- c(f, rep(0, n - length(f)))
  if (sum(r) == 0 && sum(f) == 0) return(1)
  r <- r / max(sum(r), 1)
  f <- f / max(sum(f), 1)
  max(0, 1 - sum(abs(r - f)) / 2)
}

dependency_similarity <- function(real_deps, fake_deps) {
  if (!length(real_deps)) return(1)
  real_keys <- vapply(real_deps, dependency_key, character(1L))
  fake_keys <- vapply(fake_deps, dependency_key, character(1L))
  mean(real_keys %in% fake_keys)
}

dependency_key <- function(dep) {
  paste(dep$type, dep$parent, dep$child, sep = "::")
}

key_integrity_similarity <- function(real, fake) {
  key_cols <- intersect(names(real)[vapply(names(real), key_like_name, logical(1L))], names(fake))
  if (!length(key_cols)) {
    return(NA_real_)
  }
  vals <- vapply(key_cols, function(col) duplicate_pattern_similarity(real[[col]], fake[[col]]), numeric(1L))
  mean(vals, na.rm = TRUE)
}

duplicate_pattern_similarity <- function(real, fake) {
  r <- duplicated(as_safe_character(real)) | duplicated(as_safe_character(real), fromLast = TRUE)
  f <- duplicated(as_safe_character(fake)) | duplicated(as_safe_character(fake), fromLast = TRUE)
  if (length(r) != length(f)) return(NA_real_)
  mean(r == f)
}

in_memory_privacy_issues <- function(real, fake) {
  tokens <- derive_forbidden_tokens(real)
  if (!length(tokens)) {
    return(data.frame(column = character(), token_type = character(), severity = character()))
  }
  findings <- list()
  for (col in intersect(names(real), names(fake))) {
    if (!(is.character(fake[[col]]) || is.factor(fake[[col]]))) {
      next
    }
    values <- as_safe_character(fake[[col]])
    for (token in tokens) {
      if (any(grepl(token, values, fixed = TRUE, useBytes = TRUE), na.rm = TRUE)) {
        findings[[length(findings) + 1L]] <- data.frame(
          column = col,
          token_type = token_type(token),
          severity = token_severity(token),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (!length(findings)) {
    return(data.frame(column = character(), token_type = character(), severity = character()))
  }
  unique(do.call(rbind, findings))
}

print.twinfake_validation <- function(x, ...) {
  cli::cli_text("twinfake validation")
  cli::cli_text("Schema match: {x$schema_match}; row count match: {x$row_count_match}")
  cli::cli_text("Class matches: {sum(x$class_match, na.rm = TRUE)}/{length(x$class_match)}")
  if (nrow(x$possible_privacy_issues)) {
    cli::cli_alert_warning("{nrow(x$possible_privacy_issues)} possible privacy issue{?s} found. Tokens are not printed.")
  } else {
    cli::cli_text("Possible privacy issues: 0")
  }
  invisible(x)
}
