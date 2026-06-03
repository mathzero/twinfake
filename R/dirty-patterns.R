dirty_pattern_profile <- function(x) {
  if (is.character(x)) {
    return(list(
      blank = mean(x == "", na.rm = TRUE),
      whitespace = mean(grepl("^\\s+$", x), na.rm = TRUE),
      dirty_missing = mean(is_dirty_missing_token(x), na.rm = TRUE),
      numeric_string = mean(is_numeric_string_like(x), na.rm = TRUE),
      email_like = mean(is_email_like(trimws(x)), na.rm = TRUE),
      url_like = mean(is_url_like(trimws(x)), na.rm = TRUE)
    ))
  }
  if (is.numeric(x)) {
    return(list(
      na = mean(is.na(x)),
      nan = mean(is.nan(x)),
      inf = mean(is.infinite(x) & x > 0),
      neg_inf = mean(is.infinite(x) & x < 0)
    ))
  }
  list(na = mean(is.na(x)))
}

dirty_pattern_similarity <- function(real, fake) {
  rp <- dirty_pattern_profile(real)
  fp <- dirty_pattern_profile(fake)
  keys <- intersect(names(rp), names(fp))
  if (!length(keys)) {
    return(NA_real_)
  }
  1 - mean(abs(unlist(rp[keys]) - unlist(fp[keys])), na.rm = TRUE)
}
