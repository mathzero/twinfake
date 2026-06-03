detect_dependencies <- function(data) {
  if (!is.data.frame(data) || ncol(data) < 2L) {
    return(list())
  }
  deps <- list()
  cols <- names(data)
  for (j in seq_along(cols)) {
    child <- cols[[j]]
    for (i in seq_len(j - 1L)) {
      parent <- cols[[i]]
      dep <- detect_pair_dependency(data[[parent]], data[[child]], parent, child)
      if (!is.null(dep)) {
        deps[[length(deps) + 1L]] <- dep
        break
      }
    }
  }
  deps
}

detect_pair_dependency <- function(parent_x, child_x, parent, child) {
  if (length(parent_x) != length(child_x)) {
    return(NULL)
  }
  if (vectors_same(parent_x, child_x)) {
    return(list(type = "duplicate", parent = parent, child = child))
  }
  if (is.characterish(parent_x) && is.characterish(child_x)) {
    p <- as.character(parent_x)
    c <- as.character(child_x)
    both <- !is.na(p) & !is.na(c)
    if (sum(both) < 2L) return(NULL)
    if (all(tolower(p[both]) == c[both])) {
      return(list(type = "lower", parent = parent, child = child))
    }
    if (all(toupper(p[both]) == c[both])) {
      return(list(type = "upper", parent = parent, child = child))
    }
    if (all(trimws(p[both]) == c[both])) {
      return(list(type = "trim", parent = parent, child = child))
    }
    if (all(startsWith(p[both], c[both]))) {
      return(list(type = "prefix", parent = parent, child = child))
    }
    if (all(endsWith(p[both], c[both]))) {
      return(list(type = "suffix", parent = parent, child = child))
    }
    if (is_perfect_mapping(p, c)) {
      return(list(type = "categorical_map", parent = parent, child = child))
    }
  }
  date_part <- detect_date_part(parent_x, child_x)
  if (!is.null(date_part)) {
    return(list(type = date_part, parent = parent, child = child))
  }
  if (is.numeric(parent_x) && is.numeric(child_x)) {
    both <- !is.na(parent_x) & !is.na(child_x) & is.finite(parent_x) & is.finite(child_x)
    if (sum(both) > 2L && all(round(parent_x[both]) == child_x[both])) {
      return(list(type = "round", parent = parent, child = child))
    }
  }
  NULL
}

vectors_same <- function(a, b) {
  if (length(a) != length(b)) return(FALSE)
  av <- as_safe_character(a)
  bv <- as_safe_character(b)
  same_missing <- is.na(av) == is.na(bv)
  all(same_missing) && all(av[!is.na(av)] == bv[!is.na(bv)])
}

is_characterish <- function(x) {
  is.character(x) || is.factor(x)
}

is_perfect_mapping <- function(parent, child) {
  both <- !is.na(parent) & !is.na(child)
  if (sum(both) < 3L) return(FALSE)
  parent <- parent[both]
  child <- child[both]
  split_child <- split(child, parent)
  all(vapply(split_child, function(x) length(unique(x)) == 1L, logical(1L)))
}

detect_date_part <- function(parent_x, child_x) {
  if (!inherits(parent_x, c("Date", "POSIXt")) || !(is.integer(child_x) || is.numeric(child_x) || is.character(child_x))) {
    return(NULL)
  }
  both <- !is.na(parent_x) & !is.na(child_x)
  if (sum(both) < 3L) {
    return(NULL)
  }
  child_chr <- as.character(child_x[both])
  candidates <- list(
    year = format(parent_x[both], "%Y"),
    month = format(parent_x[both], "%m"),
    day = format(parent_x[both], "%d")
  )
  for (nm in names(candidates)) {
    cand <- candidates[[nm]]
    if (all(child_chr == cand) || all(as.character(as.integer(cand)) == child_chr)) {
      return(paste0("date_", nm))
    }
  }
  NULL
}

apply_dependency <- function(dep, fake, real, child_control) {
  parent_values <- fake[[dep$parent]]
  real_child <- real[[dep$child]]
  out <- switch(
    dep$type,
    duplicate = parent_values,
    lower = tolower(as.character(parent_values)),
    upper = toupper(as.character(parent_values)),
    trim = trimws(as.character(parent_values)),
    prefix = substr(as.character(parent_values), 1L, pmax(1L, nchar(as.character(real_child)))),
    suffix = {
      pv <- as.character(parent_values)
      n <- pmax(1L, nchar(as.character(real_child)))
      substr(pv, pmax(1L, nchar(pv) - n + 1L), nchar(pv))
    },
    categorical_map = fake_mapped_child(dep, fake, real),
    date_year = as.integer(format(parent_values, "%Y")),
    date_month = as.integer(format(parent_values, "%m")),
    date_day = as.integer(format(parent_values, "%d")),
    round = round(parent_values),
    fake[[dep$child]]
  )
  out[is.na(real_child)] <- typed_missing_like(real_child, length(real_child))[is.na(real_child)]
  if (is.factor(real_child)) {
    return(factor(as.character(out), levels = unique(as.character(out[!is.na(out)])), ordered = is.ordered(real_child)))
  }
  if (is.integer(real_child) && is.numeric(out)) {
    return(as.integer(out))
  }
  out
}

fake_mapped_child <- function(dep, fake, real) {
  parent_real <- as_safe_character(real[[dep$parent]])
  child_real <- real[[dep$child]]
  child_chr <- as_safe_character(child_real)
  out <- rep(NA_character_, length(parent_real))
  both <- !is.na(parent_real) & !is.na(child_chr)
  groups <- unique(parent_real[both])
  labels <- safe_label(paste0(dep$child, "_map"), seq_along(groups))
  out[both] <- labels[match(parent_real[both], groups)]
  leftover <- is.na(out) & !is.na(child_chr)
  if (any(leftover)) {
    values <- child_chr[leftover]
    out[leftover] <- vapply(
      seq_along(values),
      function(i) fake_string_for_value(values[[i]], i, prefix = dep$child),
      character(1L)
    )
  }
  out
}
