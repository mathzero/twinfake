test_that("unsupported objects are replaced without leaking internals", {
  obj <- list(secret = structure(list(value = "Alice Secret"), class = "private_object"))
  fake <- make_fake_data(obj, seed = 5)

  expect_false(grepl("Alice Secret", object_text(fake), fixed = TRUE))
  expect_true(is.list(fake))
})
