#' Scan fake outputs for possible sensitive literal leaks
#'
#' Searches generated files and reports for forbidden literal strings supplied by
#' the user, and can derive high-risk character tokens from `real_data` in memory.
#' The forbidden token list is never written to disk and tokens are not returned
#' in the result.
#'
#' @param output_dir Folder or file to scan.
#' @param forbidden_values Optional character vector of literal strings that
#'   must not appear in outputs.
#' @param real_data Optional real data object used in memory to derive high-risk
#'   tokens such as emails, URLs, IDs, postcodes, and long unique strings.
#' @return A data frame with `file`, `column`, `token_type`, and `severity`.
#' @export
privacy_scan <- function(output_dir, forbidden_values = NULL, real_data = NULL) {
  if (!is.character(output_dir) || length(output_dir) != 1L || is.na(output_dir)) {
    cli_abort_twin("{.arg output_dir} must be a single path string.")
  }
  if (!file.exists(output_dir)) {
    cli_abort_twin("Scan target does not exist: {.path {output_dir}}.")
  }

  tokens <- unique(c(as.character(forbidden_values %||% character()), derive_forbidden_tokens(real_data)))
  tokens <- tokens[!is.na(tokens) & nchar(tokens) >= 3L]
  if (!length(tokens)) {
    return(empty_privacy_findings())
  }

  paths <- if (dir.exists(output_dir)) {
    list.files(output_dir, recursive = TRUE, full.names = TRUE, all.files = FALSE, no.. = TRUE)
  } else {
    output_dir
  }
  paths <- sort(paths[file.info(paths)$isdir %in% FALSE])

  findings <- list()
  for (path in paths) {
    file_findings <- scan_file_for_tokens(path, tokens, root = if (dir.exists(output_dir)) output_dir else dirname(output_dir))
    if (nrow(file_findings)) {
      findings[[length(findings) + 1L]] <- file_findings
    }
  }
  if (!length(findings)) {
    return(empty_privacy_findings())
  }
  unique(do.call(rbind, findings))
}

empty_privacy_findings <- function() {
  data.frame(
    file = character(),
    column = character(),
    token_type = character(),
    severity = character(),
    stringsAsFactors = FALSE
  )
}

scan_file_for_tokens <- function(path, tokens, root) {
  rel <- safe_rel_path(path, root)
  format <- file_format(path)
  if (is_supported_file(path)) {
    data <- tryCatch(read_twin_file(path, rel_path = rel)$data, error = function(e) NULL)
    if (!is.null(data)) {
      structured <- scan_object_for_tokens(data, tokens, file = rel)
      if (format %in% c("csv", "tsv", "txt")) {
        text <- scan_text_file_for_tokens(path, tokens, file = rel)
        return(unique(rbind(structured, text)))
      }
      return(structured)
    }
  }
  scan_text_file_for_tokens(path, tokens, file = rel)
}

scan_text_file_for_tokens <- function(path, tokens, file) {
  text <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"), error = function(e) character())
  if (!length(text)) {
    return(empty_privacy_findings())
  }
  findings <- list()
  for (token in tokens) {
    if (any(grepl(token, text, fixed = TRUE, useBytes = TRUE))) {
      findings[[length(findings) + 1L]] <- data.frame(
        file = file,
        column = NA_character_,
        token_type = token_type(token),
        severity = token_severity(token),
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(findings)) empty_privacy_findings() else unique(do.call(rbind, findings))
}

scan_object_for_tokens <- function(obj, tokens, file) {
  if (is.data.frame(obj)) {
    return(scan_data_frame_for_tokens(obj, tokens, file = file))
  }
  if (is.list(obj)) {
    findings <- list()
    for (nm in names(obj)) {
      found <- scan_object_for_tokens(obj[[nm]], tokens, file = paste0(file, ":", nm))
      if (nrow(found)) findings[[length(findings) + 1L]] <- found
    }
    if (!length(findings)) return(empty_privacy_findings())
    return(unique(do.call(rbind, findings)))
  }
  empty_privacy_findings()
}

scan_data_frame_for_tokens <- function(x, tokens, file) {
  findings <- list()
  for (col in names(x)) {
    if (!(is.character(x[[col]]) || is.factor(x[[col]]))) {
      next
    }
    values <- as_safe_character(x[[col]])
    for (token in tokens) {
      if (any(grepl(token, values, fixed = TRUE, useBytes = TRUE), na.rm = TRUE)) {
        findings[[length(findings) + 1L]] <- data.frame(
          file = file,
          column = col,
          token_type = token_type(token),
          severity = token_severity(token),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (!length(findings)) empty_privacy_findings() else unique(do.call(rbind, findings))
}

derive_forbidden_tokens <- function(real_data) {
  if (is.null(real_data)) {
    return(character())
  }
  if (is.data.frame(real_data)) {
    tokens <- character()
    for (col in names(real_data)) {
      x <- real_data[[col]]
      if (is.character(x) || is.factor(x)) {
        tokens <- c(tokens, high_risk_tokens(as_safe_character(x)))
      }
    }
    return(unique(tokens))
  }
  if (is.list(real_data)) {
    return(unique(unlist(lapply(real_data, derive_forbidden_tokens), use.names = FALSE)))
  }
  if (is.character(real_data) || is.factor(real_data)) {
    return(high_risk_tokens(as_safe_character(real_data)))
  }
  character()
}

high_risk_tokens <- function(x) {
  x <- unique(x[!is.na(x)])
  x <- trimws(x)
  x <- x[nchar(x) >= 4L]
  x <- x[!is_dirty_missing_token(x)]
  risky <- is_email_like(x) |
    is_url_like(x) |
    is_postcode_like(x) |
    grepl("^[[:alnum:]_. -]{6,}$", x)
  unique(x[risky])
}

token_type <- function(token) {
  if (is_email_like(token)) return("email")
  if (is_url_like(token)) return("url")
  if (is_postcode_like(token)) return("postcode")
  if (grepl("^[A-Za-z0-9_.-]{6,}$", token)) return("id_or_code")
  "literal"
}

token_severity <- function(token) {
  if (token_type(token) %in% c("email", "url", "postcode", "id_or_code")) {
    return("high")
  }
  "medium"
}
