test_that("perfect text mappings remain perfect after faking", {
  real <- toy_patients()
  fake <- make_fake_data(real, seed = 12)

  grouped <- split(fake$short_name, fake$name)
  expect_true(all(vapply(grouped, function(x) length(unique(x[!is.na(x)])) <= 1L, logical(1))))
  expect_false(any(real$name %in% fake$name))
  expect_false(any(real$short_name %in% fake$short_name))
})
