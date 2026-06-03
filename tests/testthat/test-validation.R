test_that("validate_fake_data returns a structured validation object", {
  real <- toy_patients()
  fake <- make_fake_data(real, seed = 44)
  validation <- validate_fake_data(real, fake)

  expect_s3_class(validation, "twinfake_validation")
  expect_true(validation$schema_match)
  expect_true(validation$row_count_match)
  expect_true(all(validation$class_match))
  expect_equal(nrow(validation$possible_privacy_issues), 0)
})
