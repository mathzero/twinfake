test_that("make_fake_folder recreates folder tree and writes safe manifest", {
  root <- tempdir()
  input <- make_fixture_folder(root)
  output <- file.path(root, "fake")

  result <- make_fake_folder(input, output, seed = 42, overwrite = TRUE, unknown = "empty_placeholder")

  expect_s3_class(result, "twinfake_folder_result")
  expect_true(file.exists(file.path(output, "patients.csv")))
  expect_true(file.exists(file.path(output, "nested", "appointments.rds")))
  expect_true(file.exists(file.path(output, "nested", "labs.tsv")))
  expect_true(file.exists(file.path(output, "notes.bin")))
  expect_true(file.exists(file.path(output, "twinfake_manifest.json")))

  manifest_text <- paste(readLines(file.path(output, "twinfake_manifest.json"), warn = FALSE), collapse = "\n")
  expect_false(any(vapply(sensitive_tokens(), function(tok) grepl(tok, manifest_text, fixed = TRUE), logical(1))))
})
