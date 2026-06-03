fake_vec <- function(x, ...) {
  UseMethod("fake_vec")
}

fake_vec.default <- function(
    x,
    control = list(sensitivity = "sensitive"),
    name = "value",
    preserve_row_count = TRUE,
    n = length(x),
    key_map = NULL,
    risk_level = "strict",
    ...) {
  sensitivity <- control$sensitivity %||% "sensitive"

  if (!is.null(key_map)) {
    return(fake_keyed_vec(x, key_map, template = x))
  }

  if (sensitivity == "copy") {
    return(resize_vec(x, n))
  }
  if (sensitivity == "drop") {
    return(typed_missing_like(x, n))
  }
  if (sensitivity == "hash") {
    return(restore_fake_class(safe_hash(resize_vec(x, n), salt = control$salt %||% "twinfake"), x))
  }
  if (sensitivity == "structure_only") {
    return(structure_only_vec(x, n))
  }
  if (sensitivity == "public_code") {
    return(fake_public_code_vec(x, n))
  }

  if (is.factor(x)) return(fake_factor_vec(x, n, name = name))
  if (inherits(x, "Date")) return(fake_date_vec(x, n, risk_level = risk_level))
  if (inherits(x, "POSIXt")) return(fake_posix_vec(x, n, risk_level = risk_level))
  if (inherits(x, "difftime")) return(fake_difftime_vec(x, n, risk_level = risk_level))
  if (is.integer(x)) return(as.integer(round(fake_numeric_core(x, n, risk_level = risk_level, template = x))))
  if (is.double(x)) return(fake_numeric_core(x, n, risk_level = risk_level, template = x))
  if (is.logical(x)) return(fake_logical_vec(x, n))
  if (is.character(x)) return(fake_character_vec(x, n, name = name))
  if (is.complex(x)) return(complex(real = fake_numeric_core(Re(x), n, risk_level = risk_level), imaginary = fake_numeric_core(Im(x), n, risk_level = risk_level)))
  if (is.raw(x)) return(as.raw(rep(0L, n)))
  if (is.list(x)) return(fake_list_vec(x, n))

  cli_warn_twin(
    "Unsupported object column {.field {name}} was replaced with safe placeholders."
  )
  structure_only_vec(x, n)
}

restore_fake_class <- function(x, template) {
  if (is.factor(template)) {
    return(factor(x))
  }
  if (inherits(template, "Date")) {
    return(as.Date(x, origin = "1970-01-01"))
  }
  if (inherits(template, "POSIXt")) {
    return(as.POSIXct(x, origin = "1970-01-01", tz = attr(template, "tzone") %||% "UTC"))
  }
  x
}

resize_vec <- function(x, n) {
  if (length(x) == n) return(x)
  if (!length(x)) return(typed_missing_like(x, n))
  x[rep(seq_along(x), length.out = n)]
}

typed_missing_like <- function(x, n) {
  if (is.factor(x)) {
    return(factor(rep(NA_character_, n), levels = levels(x), ordered = is.ordered(x)))
  }
  if (inherits(x, "Date")) return(as.Date(rep(NA_real_, n), origin = "1970-01-01"))
  if (inherits(x, "POSIXt")) return(as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = attr(x, "tzone") %||% "UTC"))
  if (inherits(x, "difftime")) return(as.difftime(rep(NA_real_, n), units = attr(x, "units") %||% "secs"))
  if (is.integer(x)) return(rep(NA_integer_, n))
  if (is.double(x)) return(rep(NA_real_, n))
  if (is.logical(x)) return(rep(NA, n))
  if (is.character(x)) return(rep(NA_character_, n))
  if (is.complex(x)) return(rep(NA_complex_, n))
  if (is.raw(x)) return(as.raw(rep(0L, n)))
  if (is.list(x)) return(rep(list(NULL), n))
  rep(NA, n)
}

structure_only_vec <- function(x, n) {
  out <- typed_missing_like(x, n)
  if (is.character(x)) {
    out[!is.na(resize_vec(x, n))] <- "TEXT_PLACEHOLDER"
  } else if (is.factor(x)) {
    out <- factor(rep(NA_character_, n), levels = "LEVEL_PLACEHOLDER", ordered = is.ordered(x))
    out[!is.na(resize_vec(x, n))] <- "LEVEL_PLACEHOLDER"
  } else if (is.logical(x)) {
    out[!is.na(resize_vec(x, n))] <- FALSE
  } else if (is.numeric(x) || inherits(x, "difftime")) {
    out[!is.na(resize_vec(x, n))] <- 0
  }
  out
}

fake_public_code_vec <- function(x, n) {
  if (!length(x)) return(resize_vec(x, n))
  miss <- is.na(resize_vec(x, n))
  observed <- x[!is.na(x)]
  if (!length(observed)) return(typed_missing_like(x, n))
  idx <- sample_indices(length(observed), n, replace = TRUE)
  out <- observed[idx]
  out[miss] <- typed_missing_like(x, n)[miss]
  if (is.factor(x)) {
    out <- factor(as.character(out), levels = levels(x), ordered = is.ordered(x))
  }
  out
}

fake_factor_vec <- function(x, n, name = "value") {
  source <- resize_vec(x, n)
  original_levels <- levels(x)
  fake_levels <- safe_label(paste0(name, "_level"), seq_along(original_levels))
  if (!length(fake_levels)) {
    fake_levels <- character()
  }
  idx <- as.integer(source)
  out <- rep(NA_character_, n)
  ok <- !is.na(idx) & idx >= 1L & idx <= length(fake_levels)
  out[ok] <- fake_levels[idx[ok]]
  factor(out, levels = fake_levels, ordered = is.ordered(x))
}

fake_character_vec <- function(x, n, name = "value") {
  source <- resize_vec(x, n)
  out <- rep(NA_character_, n)
  non_missing <- !is.na(source)
  uniques <- unique(source[non_missing])
  if (!length(uniques)) {
    return(out)
  }
  order <- random_permutation(length(uniques))
  fake_values <- character(length(uniques))
  for (i in seq_along(uniques)) {
    fake_values[[i]] <- fake_string_for_value(uniques[[i]], order[[i]], prefix = name)
  }
  out[non_missing] <- fake_values[match(source[non_missing], uniques)]
  out
}

fake_logical_vec <- function(x, n) {
  source <- resize_vec(x, n)
  out <- rep(FALSE, n)
  observed <- x[!is.na(x)]
  p_true <- if (length(observed)) mean(observed) else 0.5
  out <- stats::runif(n) < p_true
  out[is.na(source)] <- NA
  out
}

fake_numeric_core <- function(x, n, risk_level = "strict", template = x) {
  source <- resize_vec(template, n)
  finite <- x[is.finite(x)]
  out <- rep(0, n)
  if (length(finite) == 1L) {
    out[] <- finite[[1L]]
  } else if (length(finite) > 1L) {
    probs <- seq(0, 1, length.out = 11L)
    q <- as.numeric(stats::quantile(finite, probs = probs, na.rm = TRUE, names = FALSE, type = 7))
    if (risk_level == "strict" && length(finite) > 4L) {
      q[1L] <- q[2L]
      q[length(q)] <- q[length(q) - 1L]
    }
    u <- stats::runif(n)
    out <- stats::approx(probs, q, xout = u, rule = 2, ties = "ordered")$y
    step <- stats::median(abs(diff(unique(q))))
    if (is.finite(step) && step > 0) {
      out <- out + stats::rnorm(n, sd = step / 10)
    }
  }
  rounded_prop <- if (length(finite)) mean(abs(finite - round(finite)) < 1e-8) else 1
  if (rounded_prop > 0.8) {
    out <- round(out)
  }
  out[is.na(source)] <- NA_real_
  out[is.nan(source)] <- NaN
  out[is.infinite(source) & source > 0] <- Inf
  out[is.infinite(source) & source < 0] <- -Inf
  out
}

fake_date_vec <- function(x, n, risk_level = "strict") {
  numeric <- fake_numeric_core(as.numeric(x), n, risk_level = risk_level, template = as.numeric(x))
  as.Date(round(numeric), origin = "1970-01-01")
}

fake_posix_vec <- function(x, n, risk_level = "strict") {
  tz <- attr(x, "tzone") %||% "UTC"
  numeric <- fake_numeric_core(as.numeric(x), n, risk_level = risk_level, template = as.numeric(x))
  as.POSIXct(numeric, origin = "1970-01-01", tz = tz)
}

fake_difftime_vec <- function(x, n, risk_level = "strict") {
  units <- attr(x, "units") %||% "secs"
  numeric <- fake_numeric_core(as.numeric(x), n, risk_level = risk_level, template = as.numeric(x))
  as.difftime(numeric, units = units)
}

fake_list_vec <- function(x, n) {
  source <- resize_vec(x, n)
  lapply(seq_len(n), function(i) fake_object(source[[i]]))
}

fake_keyed_vec <- function(x, key_map, template = x) {
  y <- as_safe_character(x)
  out <- rep(NA_character_, length(y))
  ok <- !is.na(y)
  out[ok] <- unname(key_map[y[ok]])
  out[ok & is.na(out)] <- fake_character_vec(y[ok & is.na(out)], sum(ok & is.na(out)), name = "key")
  restore_key_class(out, template)
}

restore_key_class <- function(x, template) {
  if (is.factor(template)) {
    return(factor(x))
  }
  if (is.integer(template)) {
    return(as.integer(x))
  }
  if (is.double(template)) {
    return(as.numeric(x))
  }
  x
}
