read_delimited_file <- function(path, sep = ",") {
  utils::read.table(
    path,
    sep = sep,
    header = TRUE,
    quote = "\"",
    comment.char = "",
    stringsAsFactors = FALSE,
    check.names = FALSE,
    na.strings = c("NA"),
    fill = TRUE
  )
}

write_delimited_file <- function(data, path, sep = ",") {
  utils::write.table(
    data,
    file = path,
    sep = sep,
    row.names = FALSE,
    col.names = TRUE,
    quote = TRUE,
    na = "",
    qmethod = "double"
  )
  invisible(path)
}
