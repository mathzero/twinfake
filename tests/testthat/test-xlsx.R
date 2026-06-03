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
