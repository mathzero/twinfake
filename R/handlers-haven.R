read_haven_file <- function(path, format = c("sav", "dta", "sas7bdat")) {
  format <- format[[1L]]
  require_suggested("haven", paste0("to read .", format, " files"))
  out <- switch(
    format,
    sav = haven::read_sav(path),
    dta = haven::read_dta(path),
    sas7bdat = haven::read_sas(path),
    cli_abort_twin("Unsupported haven format {.val {format}}.")
  )
  as.data.frame(out, stringsAsFactors = FALSE)
}

write_haven_file <- function(data, path, format = c("sav", "dta", "sas7bdat")) {
  format <- format[[1L]]
  require_suggested("haven", paste0("to write .", format, " files"))
  switch(
    format,
    sav = haven::write_sav(data, path),
    dta = haven::write_dta(data, path),
    sas7bdat = cli_abort_twin("Writing .sas7bdat is not supported by haven; use unknown = 'empty_placeholder' or another output format."),
    cli_abort_twin("Unsupported haven format {.val {format}}.")
  )
  invisible(path)
}
