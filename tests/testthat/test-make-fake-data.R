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

test_that("permuted character columns retain original values and frequencies", {
  real <- data.frame(
    decision = c(rep("include", 7), rep("exclude", 3), NA),
    stringsAsFactors = FALSE
  )
  spec <- list(files = list(default = list(columns = list(decision = list(sensitivity = "permute")))))
  fake <- make_fake_data(real, spec = spec, seed = 10)

  expect_equal(sort(na.omit(fake$decision)), sort(na.omit(real$decision)))
  expect_equal(sum(is.na(fake$decision)), sum(is.na(real$decision)))
  expect_true(all(na.omit(fake$decision) %in% na.omit(real$decision)))
})

test_that("explicit original-value actions override key-map faking", {
  real <- data.frame(
    patient_id = c("ID001", "ID002", "ID003", "ID004"),
    stringsAsFactors = FALSE
  )
  key_map <- setNames(paste0("key_", seq_along(real$patient_id)), real$patient_id)
  spec <- list(files = list(default = list(columns = list(patient_id = list(sensitivity = "permute")))))
  fake <- twinfake:::with_twin_seed(2, twinfake:::fake_data_frame(
    real,
    spec = spec,
    key_maps = list(patient_id = key_map)
  ))

  expect_equal(sort(fake$patient_id), sort(real$patient_id))
  expect_false(any(fake$patient_id %in% key_map))
})

test_that("children of permuted duplicate parents can keep the shared permutation", {
  real <- data.frame(
    patient_id = c("ID001", "ID002", "ID001", "ID003"),
    patient_id_copy = c("ID001", "ID002", "ID001", "ID003"),
    stringsAsFactors = FALSE
  )
  spec <- list(files = list(default = list(columns = list(
    patient_id = list(sensitivity = "permute"),
    patient_id_copy = list(sensitivity = "hash")
  ))))
  fake <- make_fake_data(real, spec = spec, seed = 21)

  expect_equal(sort(fake$patient_id), sort(real$patient_id))
  expect_equal(fake$patient_id_copy, twinfake:::safe_hash(fake$patient_id))
})

test_that("sensitive mapped children follow a permuted parent without keeping raw child labels", {
  real <- data.frame(
    decision = c("include", "exclude", "include", "maybe", "exclude", "include"),
    decision_code = c("I", "E", "I", "M", "E", "I"),
    stringsAsFactors = FALSE
  )
  spec <- list(files = list(default = list(columns = list(
    decision = list(sensitivity = "permute"),
    decision_code = list(sensitivity = "sensitive")
  ))))
  fake <- make_fake_data(real, spec = spec, seed = 14)

  grouped <- split(fake$decision_code, fake$decision)
  expect_true(all(vapply(grouped, function(x) length(unique(x[!is.na(x)])) <= 1L, logical(1))))
  expect_equal(sort(fake$decision), sort(real$decision))
  expect_false(any(real$decision_code %in% fake$decision_code))
})

test_that("shared permutation propagates through dependency chains", {
  real <- data.frame(
    name = c("Alice", "Bob", "Carol", "Dina"),
    name_lower = c("alice", "bob", "carol", "dina"),
    name_lower_copy = c("alice", "bob", "carol", "dina"),
    stringsAsFactors = FALSE
  )
  spec <- list(files = list(default = list(columns = list(
    name = list(sensitivity = "permute"),
    name_lower = list(sensitivity = "sensitive"),
    name_lower_copy = list(sensitivity = "hash")
  ))))
  fake <- make_fake_data(real, spec = spec, seed = 32)

  expect_equal(fake$name_lower, tolower(fake$name))
  expect_equal(fake$name_lower_copy, twinfake:::safe_hash(fake$name_lower))
})

test_that("relationship-breaking child actions still override permuted parents", {
  real <- data.frame(
    decision = c("include", "exclude", "include", "maybe", "exclude", "include"),
    decision_code = c("I", "E", "I", "M", "E", "I"),
    stringsAsFactors = FALSE
  )
  spec <- list(files = list(default = list(columns = list(
    decision = list(sensitivity = "permute"),
    decision_code = list(sensitivity = "drop")
  ))))
  fake <- make_fake_data(real, spec = spec, seed = 14)

  expect_true(all(is.na(fake$decision_code)))
})

test_that("structure-only children are not overwritten by detected relationships", {
  real <- data.frame(
    name = c("Alice", "Bob", "Carol"),
    name_lower = c("alice", "bob", "carol"),
    stringsAsFactors = FALSE
  )
  spec <- list(files = list(default = list(columns = list(
    name_lower = list(sensitivity = "structure_only")
  ))))
  fake <- make_fake_data(real, spec = spec, seed = 9)

  expect_equal(fake$name_lower, rep("TEXT_PLACEHOLDER", nrow(real)))
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
