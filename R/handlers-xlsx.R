read_xlsx_file <- function(path, sheets = "all") {
  require_suggested("readxl", "to read .xlsx files")
  sheet_names <- readxl::excel_sheets(path)
  if (!identical(sheets, "all")) {
    sheet_names <- intersect(sheet_names, sheets)
  }
  out <- list()
  for (sheet in sheet_names) {
    out[[sheet]] <- as.data.frame(readxl::read_excel(path, sheet = sheet), stringsAsFactors = FALSE)
  }
  out
}

write_xlsx_file <- function(data, path) {
  require_suggested("writexl", "to write .xlsx files")
  writexl::write_xlsx(data, path)
  invisible(path)
}
