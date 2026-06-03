profile_vec <- function(x, ...) {
  UseMethod("profile_vec")
}

profile_vec.default <- function(
    x,
    sensitivity = "sensitive",
    public_code = FALSE,
    risk_level = "strict",
    ...) {
  cls <- class(x)
  out <- list(
    class = cls,
    typeof = typeof(x),
    length = length(x),
    names = !is.null(names(x)),
    missing = sum(is.na(x)),
    missing_prop = if (length(x)) mean(is.na(x)) else 0,
    unique = safe_unique_count(x),
    unique_rate = safe_unique_rate(x),
    duplicate_rate = safe_duplicate_rate(x),
    sortedness = safe_sortedness(x),
    constant = safe_constant(x),
    sensitivity = sensitivity,
    kind = vec_kind(x)
  )

  if (is.factor(x)) {
    counts <- tabulate(as.integer(x), nbins = length(levels(x)))
    out$factor <- list(
      n_levels = length(levels(x)),
      ordered = is.ordered(x),
      frequency = as.integer(counts),
      labels = if (isTRUE(public_code) || sensitivity == "public_code") levels(x) else NULL
    )
  } else if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    out$date_time <- profile_date_time(x, risk_level = risk_level)
  } else if (is.numeric(x) || inherits(x, "difftime")) {
    out$numeric <- profile_numeric(x, risk_level = risk_level)
  } else if (is.character(x)) {
    out$character <- profile_character(x)
  } else if (is.logical(x)) {
    out$logical <- list(
      true = sum(x %in% TRUE, na.rm = TRUE),
      false = sum(x %in% FALSE, na.rm = TRUE)
    )
  } else if (is.complex(x)) {
    out$complex <- list(
      real = profile_numeric(Re(x), risk_level = risk_level),
      imaginary = profile_numeric(Im(x), risk_level = risk_level)
    )
  } else if (is.raw(x)) {
    out$raw <- list(length = length(x))
  } else if (is.list(x)) {
    out$list <- list(length = length(x), element_classes = lapply(x, class))
  }

  class(out) <- c("twinfake_vec_profile", class(out))
  out
}

vec_kind <- function(x) {
  if (is.factor(x)) return("factor")
  if (inherits(x, "Date")) return("date")
  if (inherits(x, "POSIXt")) return("datetime")
  if (inherits(x, "difftime")) return("difftime")
  if (is.integer(x)) return("integer")
  if (is.double(x)) return("double")
  if (is.logical(x)) return("logical")
  if (is.character(x)) return("character")
  if (is.complex(x)) return("complex")
  if (is.raw(x)) return("raw")
  if (is.list(x)) return("list")
  "object"
}

safe_unique_count <- function(x) {
  if (!length(x)) return(0L)
  if (is.list(x) && !is.data.frame(x)) {
    return(length(x))
  }
  y <- x[!is.na(x)]
  if (!length(y)) return(0L)
  length(unique(as_safe_character(y)))
}

safe_unique_rate <- function(x) {
  denom <- sum(!is.na(x))
  if (!denom) return(0)
  safe_unique_count(x) / denom
}

safe_duplicate_rate <- function(x) {
  denom <- sum(!is.na(x))
  if (!denom) return(0)
  1 - safe_unique_count(x) / denom
}

safe_sortedness <- function(x) {
  if (length(x) < 2L || is.list(x) || is.complex(x) || is.raw(x)) {
    return("not_applicable")
  }
  y <- x[!is.na(x)]
  if (length(y) < 2L) {
    return("not_applicable")
  }
  if (all(y == sort(y))) return("ascending")
  if (all(y == sort(y, decreasing = TRUE))) return("descending")
  "unsorted"
}

safe_constant <- function(x) {
  safe_unique_count(x) <= 1L
}

profile_numeric <- function(x, risk_level = "strict") {
  if (inherits(x, "difftime")) {
    x <- as.numeric(x)
  }
  finite <- x[is.finite(x)]
  probs <- seq(0, 1, length.out = 11L)
  q <- if (length(finite)) {
    as.numeric(stats::quantile(finite, probs = probs, na.rm = TRUE, names = FALSE, type = 7))
  } else {
    rep(0, length(probs))
  }
  if (risk_level == "strict" && length(finite) > 4L) {
    q[1L] <- q[2L]
    q[length(q)] <- q[length(q) - 1L]
  }
  list(
    zero_prop = if (length(x)) mean(x == 0, na.rm = TRUE) else 0,
    positive_prop = if (length(x)) mean(x > 0, na.rm = TRUE) else 0,
    negative_prop = if (length(x)) mean(x < 0, na.rm = TRUE) else 0,
    nan = sum(is.nan(x)),
    inf = sum(is.infinite(x) & x > 0),
    neg_inf = sum(is.infinite(x) & x < 0),
    quantile_probs = probs,
    quantiles = q,
    centre = if (length(finite)) stats::median(finite) else 0,
    scale = if (length(finite) > 1L) stats::median(abs(finite - stats::median(finite))) else 0,
    rounded = if (length(finite)) mean(abs(finite - round(finite)) < 1e-8) else 1,
    monotonicity = safe_sortedness(x)
  )
}

profile_date_time <- function(x, risk_level = "strict") {
  numeric_x <- as.numeric(x)
  p <- profile_numeric(numeric_x, risk_level = risk_level)
  list(
    numeric = p,
    timezone = attr(x, "tzone") %||% "UTC",
    weekday_counts = table_safe(format(x, "%u")),
    month_counts = table_safe(format(x, "%m")),
    granularity = infer_time_granularity(x)
  )
}

infer_time_granularity <- function(x) {
  if (inherits(x, "Date")) {
    return("day")
  }
  y <- x[!is.na(x)]
  if (!length(y)) {
    return("unknown")
  }
  sec <- as.numeric(y)
  if (all(sec %% (24 * 3600) == 0)) return("day")
  if (all(sec %% 3600 == 0)) return("hour")
  if (all(sec %% 60 == 0)) return("minute")
  "second"
}

profile_character <- function(x) {
  y <- x[!is.na(x)]
  list(
    length_quantiles = if (length(y)) as.numeric(stats::quantile(nchar(y), seq(0, 1, length.out = 6L), names = FALSE)) else numeric(),
    empty = sum(y == ""),
    whitespace = sum(grepl("^\\s+$", y)),
    leading_space = sum(grepl("^\\s+", y)),
    trailing_space = sum(grepl("\\s+$", y)),
    email_like = sum(is_email_like(trimws(y))),
    url_like = sum(is_url_like(trimws(y))),
    postcode_like = sum(is_postcode_like(trimws(y))),
    numeric_string_like = sum(is_numeric_string_like(trimws(y))),
    dirty_missing_like = sum(is_dirty_missing_token(y)),
    case_counts = table_safe(case_pattern(y))
  )
}

table_safe <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) {
    return(list())
  }
  as.list(as.integer(table(x)))
}
