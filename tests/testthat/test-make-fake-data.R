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

test_that("low-cardinality character columns are faked as categorical values", {
  real <- data.frame(
    decision = c(rep("include", 7), rep("exclude", 3), NA),
    stringsAsFactors = FALSE
  )
  fake <- make_fake_data(real, seed = 10)

  expect_equal(nrow(fake), nrow(real))
  expect_equal(sort(as.integer(table(fake$decision, useNA = "no"))), c(3L, 7L))
  expect_false(any(c("include", "exclude") %in% fake$decision))
  expect_match(na.omit(fake$decision), "^decision_level_", all = TRUE)
  expect_equal(sum(is.na(fake$decision)), 1L)
})

test_that("public-code character columns preserve explicit labels", {
  real <- data.frame(
    decision = c(rep("include", 7), rep("exclude", 3), NA),
    stringsAsFactors = FALSE
  )
  spec <- list(files = list(default = list(columns = list(decision = list(sensitivity = "public_code")))))
  fake <- make_fake_data(real, spec = spec, seed = 10)

  expect_equal(sort(unique(na.omit(fake$decision))), c("exclude", "include"))
})

test_that("high-cardinality and structured character columns are not treated as categorical", {
  real <- data.frame(
    id = sprintf("ID%03d", seq_len(50)),
    email = paste0("person", seq_len(50), "@example.test"),
    stringsAsFactors = FALSE
  )
  fake <- make_fake_data(real, seed = 5)

  expect_false(any(real$id %in% fake$id))
  expect_false(any(real$email %in% fake$email))
  expect_false(any(grepl("^id_level_", fake$id)))
  expect_true(all(grepl("@example.invalid$", fake$email)))
})
