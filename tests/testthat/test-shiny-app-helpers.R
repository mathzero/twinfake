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
