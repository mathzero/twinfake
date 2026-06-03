test_that("dirty patterns are approximately preserved", {
  real <- toy_patients()
  fake <- make_fake_data(real, seed = 100)

  expect_gt(dirty_pattern_similarity(real$dirty_numeric_string, fake$dirty_numeric_string), 0.5)
  expect_equal(mean(is.na(real$email)), mean(is.na(fake$email)))
  expect_equal(sum(trimws(fake$email) == ""), sum(trimws(real$email) == ""))
})
