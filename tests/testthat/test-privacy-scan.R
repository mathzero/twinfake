test_that("privacy_scan finds known leaks and generated outputs avoid fixture tokens", {
  root <- tempdir()
  input <- make_fixture_folder(root)
  output <- file.path(root, "fake_scan")
  make_fake_folder(input, output, seed = 111, overwrite = TRUE)

  findings <- privacy_scan(output, forbidden_values = sensitive_tokens(), real_data = toy_patients())
  expect_equal(nrow(findings), 0)

  leak_dir <- file.path(root, "leak")
  dir.create(leak_dir, showWarnings = FALSE)
  writeLines("Alice Secret", file.path(leak_dir, "leak.txt"))
  leak_findings <- privacy_scan(leak_dir, forbidden_values = sensitive_tokens())
  expect_gt(nrow(leak_findings), 0)
  expect_false("Alice Secret" %in% names(leak_findings))
})
