test_that("perfect text mappings remain perfect after faking", {
  real <- toy_patients()
  fake <- make_fake_data(real, seed = 12)

  grouped <- split(fake$short_name, fake$name)
  expect_true(all(vapply(grouped, function(x) length(unique(x[!is.na(x)])) <= 1L, logical(1))))
  expect_false(any(real$name %in% fake$name))
  expect_false(any(real$short_name %in% fake$short_name))
})

test_that("profiling detects text relationships without helper lookup errors", {
  real <- toy_patients()
  profile <- profile_data(real)

  expect_s3_class(profile, "twinfake_profile")
  expect_true(any(vapply(profile$dependencies, function(dep) dep$type == "categorical_map", logical(1))))
})
