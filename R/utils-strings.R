safe_label <- function(prefix, i, width = NULL) {
  i <- as.integer(i)
  width <- width %||% max(3L, nchar(as.character(max(i, 1L))))
  paste0(prefix, "_", sprintf(paste0("%0", width, "d"), i))
}

safe_email <- function(i) {
  paste0("user", sprintf("%03d", as.integer(i)), "@example.invalid")
}

safe_url <- function(i) {
  paste0("https://example.invalid/path/", sprintf("%03d", as.integer(i)))
}

safe_postcode <- function(i) {
  paste0("AA", (as.integer(i) %% 9L) + 1L, " ", (as.integer(i) %% 9L) + 1L, "AA")
}

is_email_like <- function(x) {
  grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$", x)
}

is_url_like <- function(x) {
  grepl("^https?://", x, ignore.case = TRUE)
}

is_postcode_like <- function(x) {
  grepl("^[A-Z]{1,2}[0-9][A-Z0-9]?\\s*[0-9][A-Z]{2}$", trimws(x), ignore.case = TRUE)
}

is_numeric_string_like <- function(x) {
  grepl("^\\s*[-$]?[0-9][0-9,]*(\\.[0-9]+)?%?\\s*$", x)
}

is_dirty_missing_token <- function(x) {
  y <- trimws(tolower(x))
  y %in% c("", "na", "n/a", "null", ".", "-", "unknown", "missing", "none")
}

case_pattern <- function(x) {
  ifelse(
    x == toupper(x), "upper",
    ifelse(x == tolower(x), "lower",
      ifelse(grepl("^[[:upper:]][[:lower:]]+(\\s+[[:upper:]][[:lower:]]+)*$", x), "title", "mixed")
    )
  )
}

fake_string_for_value <- function(value, i, prefix = "value") {
  if (is.na(value)) {
    return(NA_character_)
  }
  value <- as.character(value)
  leading <- regmatches(value, regexpr("^\\s*", value))
  trailing <- regmatches(value, regexpr("\\s*$", value))
  core <- trimws(value)

  if (is_dirty_missing_token(value)) {
    out <- if (core == "") "MISSING_TOKEN" else "UNKNOWN_TOKEN"
  } else if (is_email_like(core)) {
    out <- safe_email(i)
  } else if (is_url_like(core)) {
    out <- safe_url(i)
  } else if (is_postcode_like(core)) {
    out <- safe_postcode(i)
  } else if (grepl("^0+[0-9]+$", core)) {
    out <- fake_leading_zero_id(core, i)
  } else if (is_numeric_string_like(core)) {
    out <- fake_numeric_string(core, i)
  } else if (grepl("^[[:alnum:]_.-]+$", core) && nchar(core) >= 4L) {
    out <- paste0(prefix, "_value_", sprintf("%03d", as.integer(i)))
  } else {
    out <- safe_label(prefix, i)
  }

  cased <- apply_case_pattern(out, case_pattern(core))
  if (identical(cased, core)) {
    cased <- apply_case_pattern(paste0(prefix, "_fake_", sprintf("%03d", as.integer(i))), case_pattern(core))
  }
  paste0(leading, cased, trailing)
}

fake_leading_zero_id <- function(value, i) {
  width <- nchar(value)
  modulus <- 10^min(width, 8L)
  fake_num <- (as.integer(i) + 137L) %% modulus
  original_num <- suppressWarnings(as.integer(value))
  if (!is.na(original_num) && fake_num == original_num) {
    fake_num <- (fake_num + 1L) %% modulus
  }
  sprintf(paste0("%0", width, "d"), fake_num)
}

fake_numeric_string <- function(x, i) {
  has_currency <- grepl("^\\s*[$]", x)
  has_percent <- grepl("%\\s*$", x)
  has_comma <- grepl(",", x)
  decimals <- if (grepl("\\.", x)) nchar(sub("^.*\\.([0-9]+).*$", "\\1", x)) else 0L
  value <- as.numeric(i) * 7.3 + 11
  core <- format(round(value, decimals), nsmall = decimals, trim = TRUE, scientific = FALSE)
  if (has_comma) {
    parts <- strsplit(core, ".", fixed = TRUE)[[1L]]
    parts[1L] <- format(as.numeric(parts[1L]), big.mark = ",", scientific = FALSE, trim = TRUE)
    core <- paste(parts, collapse = ".")
  }
  if (has_currency) {
    core <- paste0("$", core)
  }
  if (has_percent) {
    core <- paste0(core, "%")
  }
  core
}

apply_case_pattern <- function(x, pattern) {
  switch(
    pattern,
    upper = toupper(x),
    lower = tolower(x),
    title = paste0(toupper(substr(x, 1L, 1L)), tolower(substr(x, 2L, nchar(x)))),
    x
  )
}

safe_hash <- function(x, salt = "twinfake") {
  y <- as_safe_character(x)
  miss <- is.na(y)
  out <- character(length(y))
  out[miss] <- NA_character_
  for (i in which(!miss)) {
    bytes <- utf8ToInt(paste0(salt, "::", y[[i]]))
    val <- sum(bytes * seq_along(bytes)) %% 1000000007
    out[[i]] <- paste0("hash_", sprintf("%010d", val))
  }
  out
}
