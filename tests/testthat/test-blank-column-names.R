test_that("profiling and faking tolerate blank column names", {
  real <- data.frame(
    named = c("a", "b"),
    c("x", "y"),
    c("m", "n"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  names(real) <- c("named", "", "")

  profile <- profile_data(real)
  rows <- twinfake:::profile_columns_table(list(files = list(example = profile)))
  fake <- make_fake_data(real, seed = 1)

  expect_equal(profile$column_names, names(real))
  expect_equal(rows$column, c("named", "...2", "...3"))
  expect_equal(names(fake), names(real))
  expect_equal(nrow(fake), nrow(real))
})
