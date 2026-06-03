test_that("same seed is identical and different seeds differ", {
  real <- toy_patients()
  fake_a <- make_fake_data(real, seed = 123)
  fake_b <- make_fake_data(real, seed = 123)
  fake_c <- make_fake_data(real, seed = 124)

  expect_equal(fake_a, fake_b)
  expect_false(identical(fake_a, fake_c))
})
