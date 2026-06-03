read_arrow_file <- function(path, format = c("parquet", "feather")) {
  format <- format[[1L]]
  require_suggested("arrow", paste0("to read .", format, " files"))
  if (format == "parquet") {
    return(as.data.frame(arrow::read_parquet(path)))
  }
  as.data.frame(arrow::read_feather(path))
}

write_arrow_file <- function(data, path, format = c("parquet", "feather")) {
  format <- format[[1L]]
  require_suggested("arrow", paste0("to write .", format, " files"))
  if (format == "parquet") {
    arrow::write_parquet(data, path)
  } else {
    arrow::write_feather(data, path)
  }
  invisible(path)
}
