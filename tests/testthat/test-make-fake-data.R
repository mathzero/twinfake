test_that("make_fake_data preserves schema and broad classes", {
  real <- toy_patients()
  fake <- make_fake_data(real, seed = 20260603)

  expect_identical(names(fake), names(real))
  expect_equal(nrow(fake), nrow(real))
  expect_s3_class(fake$dob, "Date")
  expect_true(is.integer(fake$age))
  expect_true(is.factor(fake$sex))
  expect_false(any(vapply(sensitive_tokens(), function(tok) grepl(tok, object_text(fake), fixed = TRUE), logical(1))))
})

test_that("public-code columns preserve allowed labels only when configured", {
  real <- toy_patients()
  spec <- list(files = list(default = list(columns = list(sex = list(sensitivity = "public_code")))))
  fake_default <- make_fake_data(real, seed = 1)
  fake_public <- make_fake_data(real, spec = spec, seed = 1)

  expect_false(any(levels(real$sex) %in% levels(fake_default$sex)))
  expect_true(all(levels(real$sex) %in% levels(fake_public$sex)))
})
