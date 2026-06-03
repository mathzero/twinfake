with_twin_seed <- function(seed, code) {
  if (is.null(seed)) {
    return(force(code))
  }

  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed)) {
    cli_abort_twin("{.arg seed} must be `NULL` or a single numeric value.")
  }

  seed <- as.integer(seed)
  old_seed_exists <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (old_seed_exists) {
    old_seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  }

  on.exit({
    if (old_seed_exists) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  set.seed(seed)
  force(code)
}

sample_indices <- function(n, size, replace = TRUE, prob = NULL) {
  if (n <= 0L || size <= 0L) {
    return(integer())
  }
  sample.int(n = n, size = size, replace = replace, prob = prob)
}

safe_prob <- function(counts) {
  counts <- as.numeric(counts)
  if (!length(counts) || sum(counts) <= 0) {
    return(rep(1 / length(counts), length(counts)))
  }
  counts / sum(counts)
}

random_permutation <- function(n) {
  if (n <= 1L) seq_len(n) else sample.int(n)
}
