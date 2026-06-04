test_that("specs round-trip without fixture sensitive tokens", {
  spec <- list(
    defaults = list(sensitivity = "sensitive", engine = "pipeline", risk_level = "strict"),
    files = list(
      "patients.csv" = list(columns = list(
        patient_id = list(role = "key", sensitivity = "sensitive"),
        sex = list(sensitivity = "public_code")
      ))
    )
  )
  path <- file.path(tempdir(), "twinfake_spec.json")
  write_twin_spec(spec, path)
  read_back <- read_twin_spec(path)

  expect_s3_class(read_back, "twinfake_spec")
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_false(any(vapply(sensitive_tokens(), function(tok) grepl(tok, text, fixed = TRUE), logical(1))))
})

test_that("permute is a supported explicit sensitivity class", {
  spec <- list(files = list(default = list(columns = list(status = list(sensitivity = "permute")))))
  control <- twinfake:::column_control(spec, "status")

  expect_equal(control$sensitivity, "permute")
  expect_true("permute" %in% twinfake:::valid_sensitivities())
})
