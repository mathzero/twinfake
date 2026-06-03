test_that("xlsx handler processes all sheets and preserves sheet names", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  root <- tempdir()
  input <- file.path(root, "private_xlsx")
  output <- file.path(root, "fake_xlsx")
  dir.create(input, recursive = TRUE, showWarnings = FALSE)
  writexl::write_xlsx(
    list(patients = toy_patients(), labs = toy_labs()),
    file.path(input, "workbook.xlsx")
  )

  make_fake_folder(input, output, seed = 10, overwrite = TRUE)
  sheets <- readxl::excel_sheets(file.path(output, "workbook.xlsx"))
  expect_equal(sheets, c("patients", "labs"))

  fake_patients <- readxl::read_excel(file.path(output, "workbook.xlsx"), sheet = "patients")
  expect_equal(nrow(fake_patients), nrow(toy_patients()))
  expect_false(any(vapply(sensitive_tokens(), function(tok) grepl(tok, object_text(fake_patients), fixed = TRUE), logical(1))))
})

test_that("xls inputs are read and written as xlsx outputs", {
  skip_if_not_installed("readxl")
  skip_if_not_installed("writexl")

  source_xls <- readxl::readxl_example("datasets.xls")
  skip_if(!file.exists(source_xls), "readxl datasets.xls example is unavailable")

  root <- tempdir()
  input <- file.path(root, "private_xls")
  output <- file.path(root, "fake_xls")
  dir.create(input, recursive = TRUE, showWarnings = FALSE)
  file.copy(source_xls, file.path(input, "legacy.xls"), overwrite = TRUE)

  result <- make_fake_folder(input, output, seed = 11, overwrite = TRUE)
  output_workbook <- file.path(output, "legacy.xlsx")

  expect_true(file.exists(output_workbook))
  expect_false(file.exists(file.path(output, "legacy.xls")))
  expect_equal(readxl::excel_sheets(output_workbook), readxl::excel_sheets(source_xls))
  expect_equal(result$files[[1]]$format, "xls")
  expect_equal(result$files[[1]]$output_format, "xlsx")
  expect_match(paste(result$files[[1]]$warnings, collapse = " "), "Legacy \\.xls")

  output2 <- file.path(root, "fake_xls_renamed")
  result2 <- make_fake_folder(input, output2, seed = 12, overwrite = TRUE, preserve_file_names = FALSE)
  expect_true(file.exists(file.path(output2, "file_001.xlsx")))
  expect_equal(result2$files[[1]]$output_format, "xlsx")
})
