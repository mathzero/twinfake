fake_object <- function(x, ...) {
  UseMethod("fake_object")
}

fake_object.default <- function(x, ...) {
  if (is_atomicish(x)) {
    return(fake_vec(x, ...))
  }
  if (is.matrix(x) || is.array(x)) {
    out <- fake_vec(as.vector(x), n = length(x), name = "array_value")
    dim(out) <- dim(x)
    dimnames(out) <- dimnames(x)
    return(out)
  }
  if (is.list(x)) {
    return(lapply(x, fake_object, ...))
  }
  cli_warn_twin("Unsupported object of class {.cls {class(x)}} was replaced with a structure-only placeholder.")
  list(
    twinfake_placeholder = TRUE,
    original_class = class(x),
    original_type = typeof(x),
    original_dim = safe_dim(x)
  )
}

fake_object.data.frame <- function(x, ...) {
  fake_data_frame(x, ...)
}

fake_object.list <- function(x, ...) {
  out <- lapply(x, fake_object, ...)
  names(out) <- names(x)
  out
}
