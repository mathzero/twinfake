toy_patients <- function() {
  data.frame(
    patient_id = c("001", "002", "002", "NHS999999", NA, "0005"),
    name = c("Alice Secret", "Bob Confidential", "Bob Confidential", "Charlie Hidden", "Alice Secret", "Delta Private"),
    short_name = c("ALI", "BOB", "BOB", "CHA", "ALI", "DEL"),
    dob = as.Date(c("1980-01-02", "1975-03-04", "1975-03-04", "1991-05-06", NA, "2000-12-31")),
    age = c(46L, 51L, 51L, 35L, NA, 25L),
    sex = factor(c("F", "M", "M", "F", NA, "X")),
    postcode = c("ZX99 1ZZ", "AA11 1AA", "AA11 1AA", "BB22 2BB", "", "CC33 3CC"),
    email = c("alice.secret@example.test", "bob.confidential@example.test", "bob.confidential@example.test", NA, " ", "delta@example.test"),
    status = factor(c("Active", "Paused", "Paused", "Active", "N/A", "Unknown")),
    dirty_numeric_string = c("1,000", "$12.50", "N/A", " - ", "99%", "003"),
    date_string = c("2026-01-01", "01/02/2026", "impossible-date", "", "N/A", "2026"),
    notes = c("Do not leak Alice Secret", "", "  ", "NULL", "unknown", "contains NHS999999"),
    stringsAsFactors = FALSE
  )
}

toy_appointments <- function() {
  data.frame(
    patient_id = c("001", "002", "002", "999", "NHS999999", NA),
    appointment_id = c("A001", "A002", "A003", "A004", "A005", "A006"),
    date = as.Date(c("2026-01-10", "2026-01-11", "2026-01-12", "2026-01-13", "2026-01-14", NA)),
    status = c("attended", "missed", "attended", "cancelled", "attended", "N/A"),
    clinician_code = c("GP1", "GP2", "GP2", "GP3", "GP1", "GPX"),
    stringsAsFactors = FALSE
  )
}

toy_labs <- function() {
  data.frame(
    patient_id = c("001", "002", "002", "NHS999999", "999"),
    lab_name = c("Hb", "K", "K", "Na", "Hb"),
    lab_value = c(12.1, 4.2, NaN, Inf, -Inf),
    unit = c("g/dL", "mmol/L", "mmol/L", "mmol/L", "g/dL"),
    abnormal_flag = c(FALSE, FALSE, TRUE, TRUE, NA),
    stringsAsFactors = FALSE
  )
}

sensitive_tokens <- function() {
  c(
    "Alice Secret",
    "Bob Confidential",
    "NHS999999",
    "ZX99 1ZZ",
    "alice.secret@example.test"
  )
}

object_text <- function(x) {
  paste(capture.output(str(x)), collapse = "\n")
}

make_fixture_folder <- function(root) {
  dir.create(file.path(root, "private", "nested"), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(toy_patients(), file.path(root, "private", "patients.csv"), row.names = FALSE)
  saveRDS(toy_appointments(), file.path(root, "private", "nested", "appointments.rds"))
  utils::write.table(toy_labs(), file.path(root, "private", "nested", "labs.tsv"), sep = "\t", row.names = FALSE, quote = TRUE)
  writeLines("raw secret sidecar", file.path(root, "private", "notes.bin"))
  file.path(root, "private")
}
