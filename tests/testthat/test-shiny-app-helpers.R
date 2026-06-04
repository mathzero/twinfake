test_that("Shiny app tables and summary render from cached profiles", {
  profile <- list(
    files = list(example = profile_data(toy_patients())),
    skipped = list(),
    risk_level = "strict"
  )
  class(profile) <- c("twinfake_folder_profile", class(profile))

  files <- twinfake:::profile_files_table(profile)
  columns <- twinfake:::profile_columns_table(profile)
  summary <- twinfake:::profile_summary_ui(profile, elapsed = 3.2)

  expect_equal(nrow(files), 1)
  expect_gt(nrow(columns), 1)
  expect_s3_class(summary, "shiny.tag")
})

test_that("Shiny column actions feed display tables and specs", {
  folder_profile <- list(
    files = list(example = profile_data(toy_patients())),
    skipped = list(),
    risk_level = "strict"
  )
  class(folder_profile) <- c("twinfake_folder_profile", class(folder_profile))

  controls <- twinfake:::profile_column_controls(folder_profile, default_sensitivity = "sensitive")
  idx <- which(controls$column == "sex")
  controls$sensitivity[[idx]] <- "permute"
  controls$edited[[idx]] <- TRUE

  columns <- twinfake:::profile_columns_with_controls(folder_profile, controls)
  spec <- twinfake:::spec_from_column_controls(folder_profile, controls)

  expect_true("ref" %in% names(columns))
  expect_equal(columns$sensitivity[columns$column == "sex"], "permute")
  expect_true(columns$custom_action[columns$column == "sex"])
  expect_equal(spec$files[["example"]]$columns$sex$sensitivity, "permute")
})

test_that("Shiny column table selections apply actions to multiple controls", {
  folder_profile <- list(
    files = list(example = profile_data(toy_patients())),
    skipped = list(),
    risk_level = "strict"
  )
  class(folder_profile) <- c("twinfake_folder_profile", class(folder_profile))
  controls <- twinfake:::profile_column_controls(folder_profile, default_sensitivity = "sensitive")
  columns <- twinfake:::profile_columns_with_controls(folder_profile, controls)
  selected_rows <- match(c("sex", "name"), columns$column)
  refs <- twinfake:::selected_column_refs(columns, selected_rows)
  result <- twinfake:::apply_column_action_to_controls(controls, refs, "hash")

  expect_equal(length(refs), 2)
  expect_equal(result$count, 2)
  expect_equal(result$controls$sensitivity[selected_rows], c("hash", "hash"))
  expect_true(all(result$controls$edited[selected_rows]))
  expect_identical(twinfake:::selected_column_refs(columns, integer()), character())
  expect_s3_class(twinfake:::selected_column_summary_ui(columns, selected_rows), "shiny.tag")
})

test_that("Shiny relationship table shows generated tie status", {
  folder_profile <- list(
    files = list(example = profile_data(toy_patients())),
    skipped = list(),
    risk_level = "strict"
  )
  class(folder_profile) <- c("twinfake_folder_profile", class(folder_profile))
  controls <- twinfake:::profile_column_controls(folder_profile, default_sensitivity = "sensitive")

  deps <- twinfake:::profile_dependencies_with_controls(folder_profile, controls)
  controls$sensitivity[controls$column == deps$child[[1L]]] <- "permute"
  deps_overridden <- twinfake:::profile_dependencies_with_controls(folder_profile, controls)
  controls$sensitivity[controls$column == deps$parent[[1L]]] <- "permute"
  controls$sensitivity[controls$column == deps$child[[1L]]] <- "hash"
  deps_shared <- twinfake:::profile_dependencies_with_controls(folder_profile, controls)

  expect_gt(nrow(deps), 0)
  expect_true(any(deps$tied_when_generated == "yes"))
  expect_true(any(deps_overridden$tied_when_generated == "overridden"))
  expect_true(any(deps_shared$tied_when_generated == "shared permutation"))
})

test_that("Shiny action help covers every sensitivity option", {
  details <- twinfake:::sensitivity_action_details()

  expect_equal(details$sensitivity, twinfake:::valid_sensitivities())
  expect_true(all(nzchar(details$label)))
  expect_true(all(nzchar(details$effect)))
  expect_true(all(nzchar(details$disclosure)))
  expect_s3_class(twinfake:::selected_sensitivity_action_ui("permute"), "shiny.tag")
  expect_s3_class(twinfake:::sensitivity_action_guide_ui(), "shiny.tag")
})

test_that("Shiny error messages strip CLI ANSI escapes", {
  err <- simpleError("\033[34mwritexl\033[39m is missing")

  expect_equal(twinfake:::shiny_condition_message(err), "writexl is missing")
})

test_that("generation preflight maps output formats to install hints", {
  formats <- c("csv", "xlsx", "xls", "parquet", "dta")
  packages <- twinfake:::required_output_packages_for_formats(formats)

  expect_equal(packages, c("writexl", "arrow", "haven"))
  expect_equal(
    twinfake:::install_packages_call(c("writexl", "arrow")),
    'install.packages(c("writexl", "arrow"))'
  )
})

test_that("profile progress callback is called once per discovered file", {
  root <- tempdir()
  input <- make_fixture_folder(root)
  seen <- character()

  profile <- twinfake:::profile_folder_impl(
    input,
    progress = function(i, total, rel) {
      seen <<- c(seen, rel)
    }
  )

  expect_s3_class(profile, "twinfake_folder_profile")
  expect_equal(length(seen), 4)
})
