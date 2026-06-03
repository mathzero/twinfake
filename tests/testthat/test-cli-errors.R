test_that("CLI wrappers evaluate caller variables in errors and warnings", {
  missing <- file.path(tempdir(), "definitely_missing_twinfake_dir")
  expect_error(
    twinfake:::check_dir_readable(missing, "input_dir"),
    "input_dir.*does not exist"
  )
})

test_that("directory checks trim accidental path whitespace when possible", {
  path <- tempdir()
  expect_warning(
    checked <- twinfake:::check_dir_readable(paste0(path, " "), "input_dir"),
    "whitespace"
  )
  expect_identical(checked, path)
})
