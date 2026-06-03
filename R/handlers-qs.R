read_qs_file <- function(path) {
  require_suggested("qs", "to read .qs files")
  qs::qread(path)
}

write_qs_file <- function(data, path) {
  require_suggested("qs", "to write .qs files")
  qs::qsave(data, path)
  invisible(path)
}
