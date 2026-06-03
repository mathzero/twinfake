test_that("cross-file keys preserve joins within a folder run", {
  root <- tempdir()
  input <- make_fixture_folder(root)
  output <- file.path(root, "fake_keys")

  make_fake_folder(input, output, seed = 99, overwrite = TRUE)

  real_patients <- toy_patients()
  real_appointments <- toy_appointments()
  fake_patients <- utils::read.csv(file.path(output, "patients.csv"), stringsAsFactors = FALSE)
  fake_appointments <- readRDS(file.path(output, "nested", "appointments.rds"))

  real_key <- "002"
  expected_fake <- fake_patients$patient_id[match(real_key, real_patients$patient_id)]
  observed_fake <- unique(fake_appointments$patient_id[real_appointments$patient_id == real_key])
  expect_equal(observed_fake, expected_fake)
})
