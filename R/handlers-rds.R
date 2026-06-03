read_rds_file <- function(path) {
  readRDS(path)
}

write_rds_file <- function(data, path) {
  saveRDS(data, path)
  invisible(path)
}
