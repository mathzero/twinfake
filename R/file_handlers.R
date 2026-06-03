supported_extensions <- function() {
  c(
    "csv", "tsv", "txt",
    "rds", "rdata", "rda",
    "xls", "xlsx",
    "parquet", "feather",
    "sav", "dta", "sas7bdat",
    "qs"
  )
}

file_format <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "rda") "rdata" else ext
}

is_supported_file <- function(path) {
  file_format(path) %in% supported_extensions()
}

read_twin_file <- function(path, rel_path = basename(path), xlsx_sheets = "all") {
  format <- file_format(path)
  data <- switch(
    format,
    csv = read_delimited_file(path, sep = ","),
    tsv = read_delimited_file(path, sep = "\t"),
    txt = read_delimited_file(path, sep = "\t"),
    rds = read_rds_file(path),
    rdata = read_rdata_file(path),
    xls = read_xlsx_file(path, sheets = xlsx_sheets),
    xlsx = read_xlsx_file(path, sheets = xlsx_sheets),
    parquet = read_arrow_file(path, format = "parquet"),
    feather = read_arrow_file(path, format = "feather"),
    sav = read_haven_file(path, format = "sav"),
    dta = read_haven_file(path, format = "dta"),
    sas7bdat = read_haven_file(path, format = "sas7bdat"),
    qs = read_qs_file(path),
    cli_abort_twin("Unsupported file type for {.path {path}}.")
  )

  type <- if (format %in% c("xls", "xlsx")) {
    "excel"
  } else if (format == "rdata") {
    "rdata"
  } else if (is.data.frame(data)) {
    "table"
  } else {
    "object"
  }

  list(
    source_path = path,
    rel_path = rel_path,
    format = format,
    type = type,
    data = data,
    warnings = if (format == "xls") {
      "Legacy .xls input is read with readxl and written as .xlsx because writexl does not write .xls workbooks."
    } else {
      character()
    }
  )
}

write_twin_file <- function(entry, data, output_path, overwrite = FALSE) {
  if (file.exists(output_path) && !overwrite) {
    cli_abort_twin("Refusing to overwrite existing file: {.path {output_path}}.")
  }
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  format <- entry$format
  switch(
    format,
    csv = write_delimited_file(data, output_path, sep = ","),
    tsv = write_delimited_file(data, output_path, sep = "\t"),
    txt = write_delimited_file(data, output_path, sep = "\t"),
    rds = write_rds_file(data, output_path),
    rdata = write_rdata_file(data, output_path),
    xls = {
      if (file_format(output_path) != "xlsx") {
        cli_abort_twin("Legacy .xls inputs must be written to a {.file .xlsx} output path.")
      }
      write_xlsx_file(data, output_path)
    },
    xlsx = write_xlsx_file(data, output_path),
    parquet = write_arrow_file(data, output_path, format = "parquet"),
    feather = write_arrow_file(data, output_path, format = "feather"),
    sav = write_haven_file(data, output_path, format = "sav"),
    dta = write_haven_file(data, output_path, format = "dta"),
    sas7bdat = write_haven_file(data, output_path, format = "sas7bdat"),
    qs = write_qs_file(data, output_path),
    cli_abort_twin("Unsupported output file type for {.path {output_path}}.")
  )
  invisible(output_path)
}

fake_entry_data <- function(entry, spec, key_maps, preserve_row_count, engine, risk_level, quiet) {
  if (entry$type == "table") {
    return(fake_data_frame(
      entry$data,
      spec = spec,
      file_id = entry$rel_path,
      preserve_row_count = preserve_row_count,
      engine = engine,
      risk_level = risk_level,
      key_maps = key_maps,
      quiet = quiet
    ))
  }
  if (entry$type == "excel") {
    out <- list()
    for (sheet in names(entry$data)) {
      out[[sheet]] <- fake_data_frame(
        entry$data[[sheet]],
        spec = spec,
        file_id = entry$rel_path,
        sheet = sheet,
        preserve_row_count = preserve_row_count,
        engine = engine,
        risk_level = risk_level,
        key_maps = key_maps,
        quiet = quiet
      )
    }
    return(out)
  }
  if (entry$type == "rdata") {
    out <- list()
    for (nm in names(entry$data)) {
      if (is.data.frame(entry$data[[nm]])) {
        out[[nm]] <- fake_data_frame(
          entry$data[[nm]],
          spec = spec,
          file_id = entry$rel_path,
          sheet = nm,
          preserve_row_count = preserve_row_count,
          engine = engine,
          risk_level = risk_level,
          key_maps = key_maps,
          quiet = quiet
        )
      } else {
        out[[nm]] <- fake_object(
          entry$data[[nm]],
          spec = spec,
          preserve_row_count = preserve_row_count,
          engine = engine,
          risk_level = risk_level,
          quiet = quiet
        )
      }
    }
    return(out)
  }
  fake_object(
    entry$data,
    spec = spec,
    preserve_row_count = preserve_row_count,
    engine = engine,
    risk_level = risk_level,
    quiet = quiet
  )
}

safe_manifest_for_entry <- function(entry, output_path, output_dir = dirname(output_path), fake_data = NULL, status = "generated", warnings = character()) {
  base <- list(
    input_path = entry$rel_path,
    output_path = if (is.na(output_path)) NA_character_ else safe_rel_path(output_path, output_dir),
    format = entry$format,
    output_format = if (is.na(output_path)) NA_character_ else file_format(output_path),
    type = entry$type,
    status = status,
    warnings = unique(c(entry$warnings %||% character(), warnings))
  )

  if (is.data.frame(fake_data)) {
    base$row_count <- nrow(fake_data)
    base$column_names <- names(fake_data)
    base$classes <- lapply(fake_data, class)
  } else if (entry$type == "excel" && is.list(fake_data)) {
    base$sheets <- lapply(fake_data, function(sheet_data) {
      list(
        row_count = if (is.data.frame(sheet_data)) nrow(sheet_data) else NA_integer_,
        column_names = if (is.data.frame(sheet_data)) names(sheet_data) else character(),
        classes = if (is.data.frame(sheet_data)) lapply(sheet_data, class) else list()
      )
    })
  } else if (entry$type == "rdata" && is.list(fake_data)) {
    base$objects <- lapply(fake_data, function(obj) {
      list(
        class = class(obj),
        dim = safe_dim(obj),
        column_names = if (is.data.frame(obj)) names(obj) else character()
      )
    })
  } else {
    base$class <- class(fake_data)
    base$dim <- safe_dim(fake_data)
  }
  base
}

write_manifest <- function(entries, output_dir, seed, engine, risk_level) {
  manifest <- list(
    package = "twinfake",
    package_version = package_version_string(),
    generated_at_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    seed = seed,
    engine = engine,
    risk_level = risk_level,
    privacy_notice = "Manifest intentionally excludes raw values, original factor labels, sample rows, fitted models, and real-to-fake key maps.",
    files = entries
  )
  path <- file.path(output_dir, "twinfake_manifest.json")
  jsonlite::write_json(manifest, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  path
}
