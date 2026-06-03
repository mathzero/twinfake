read_rdata_file <- function(path) {
  env <- new.env(parent = emptyenv())
  load(path, envir = env)
  out <- as.list(env)
  out[sort(names(out))]
}

write_rdata_file <- function(data, path) {
  env <- new.env(parent = emptyenv())
  for (nm in names(data)) {
    assign(nm, data[[nm]], envir = env)
  }
  save(list = names(data), file = path, envir = env)
  invisible(path)
}
