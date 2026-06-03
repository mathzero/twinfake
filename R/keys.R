key_like_name <- function(name) {
  grepl("(^id$|_id$|id$|_key$|key$|code$|nhs_number$|number$)", name, ignore.case = TRUE)
}

build_key_maps <- function(entries, spec = NULL) {
  occurrences <- collect_key_occurrences(entries, spec = spec)
  maps <- list()
  for (key_name in names(occurrences)) {
    values <- unique(unlist(occurrences[[key_name]], use.names = FALSE))
    values <- values[!is.na(values)]
    if (!length(values)) {
      next
    }
    maps[[key_name]] <- stats::setNames(
      make_fake_key_values(values),
      values
    )
  }
  maps
}

collect_key_occurrences <- function(entries, spec = NULL) {
  out <- list()
  explicit <- spec_key_columns(spec)
  for (key_name in names(explicit)) {
    out[[key_name]] <- character()
  }

  for (entry in entries) {
    for (tbl in entry_tables(entry)) {
      if (!is.data.frame(tbl$data)) next
      file_id <- tbl$file_id
      sheet <- tbl$sheet
      for (col in names(tbl$data)) {
        ref <- column_ref(file_id, col, sheet)
        explicit_key <- NULL
        for (key_name in names(explicit)) {
          if (ref %in% explicit[[key_name]] || paste0(basename(file_id), ":", col) %in% explicit[[key_name]] || col %in% explicit[[key_name]]) {
            explicit_key <- key_name
          }
        }
        auto_key <- is.null(explicit_key) && key_like_name(col)
        if (!auto_key && is.null(explicit_key)) {
          next
        }
        key_name <- explicit_key %||% col
        out[[key_name]] <- c(out[[key_name]], as_safe_character(tbl$data[[col]]))
      }
    }
  }

  repeated <- repeated_column_names(entries)
  for (col in names(repeated)) {
    if (!key_like_name(col) || length(repeated[[col]]) < 2L) next
    out[[col]] <- unique(c(out[[col]], unlist(repeated[[col]], use.names = FALSE)))
  }
  out
}

repeated_column_names <- function(entries) {
  out <- list()
  for (entry in entries) {
    for (tbl in entry_tables(entry)) {
      if (!is.data.frame(tbl$data)) next
      for (col in names(tbl$data)) {
        out[[col]] <- c(out[[col]], as_safe_character(tbl$data[[col]]))
      }
    }
  }
  out[vapply(out, function(x) length(unique(x[!is.na(x)])) > 0L, logical(1L))]
}

entry_tables <- function(entry) {
  if (entry$type == "table") {
    return(list(list(file_id = entry$rel_path, sheet = NULL, data = entry$data)))
  }
  if (entry$type == "excel") {
    out <- list()
    for (sheet in names(entry$data)) {
      out[[sheet]] <- list(file_id = entry$rel_path, sheet = sheet, data = entry$data[[sheet]])
    }
    return(out)
  }
  if (entry$type == "rdata") {
    out <- list()
    for (nm in names(entry$data)) {
      if (is.data.frame(entry$data[[nm]])) {
        out[[nm]] <- list(file_id = entry$rel_path, sheet = nm, data = entry$data[[nm]])
      }
    }
    return(out)
  }
  list()
}

make_fake_key_values <- function(values) {
  out <- character(length(values))
  for (i in seq_along(values)) {
    value <- values[[i]]
    if (is_email_like(value)) {
      out[[i]] <- safe_email(i)
    } else if (is_postcode_like(value)) {
      out[[i]] <- safe_postcode(i)
    } else if (grepl("^0+[0-9]+$", value)) {
      out[[i]] <- fake_leading_zero_id(value, i)
    } else if (grepl("^[0-9]+$", value)) {
      out[[i]] <- as.character(100000L + i)
    } else if (!is.na(suppressWarnings(as.numeric(value)))) {
      out[[i]] <- as.character(100000L + i)
    } else {
      out[[i]] <- paste0("key_", sprintf("%05d", i))
    }
  }
  out
}

key_map_for_column <- function(key_maps, column) {
  if (is.null(key_maps)) return(NULL)
  key_maps[[column]]
}
