#' Launch the local twinfake Shiny app
#'
#' Starts a local-only Shiny app for reviewing folder structure, safe column
#' summaries, key suggestions, and sensitivity controls. The app does not display
#' raw values by default and does not save raw values in specs.
#'
#' @param input_dir Input folder to inspect.
#' @param output_dir Optional output folder for fake files.
#' @param spec_path Optional JSON spec path to load or write.
#' @param host Host interface. Defaults to `"127.0.0.1"` and must be local.
#' @param port Optional port.
#' @return The value returned by `shiny::runApp()`.
#' @export
launch_twin_app <- function(
    input_dir,
    output_dir = NULL,
    spec_path = NULL,
    host = "127.0.0.1",
    port = NULL) {
  require_suggested("shiny", "to launch the local twinfake app")
  require_suggested("DT", "to launch the local twinfake app")
  input_dir <- check_dir_readable(input_dir, "input_dir")
  if (!host %in% c("127.0.0.1", "localhost")) {
    cli_abort_twin("The twinfake app is local-only; {.arg host} must be {.val 127.0.0.1} or {.val localhost}.")
  }
  input_dir <- normalizePath(input_dir, winslash = "/", mustWork = TRUE)
  output_dir <- output_dir %||% file.path(dirname(input_dir), "fake")
  spec_path <- spec_path %||% file.path(output_dir, "twinfake_spec.json")

  app <- shiny::shinyApp(
    ui = shiny_app_ui(input_dir, output_dir, spec_path),
    server = shiny_app_server(input_dir, output_dir, spec_path)
  )
  args <- list(appDir = app, host = host, launch.browser = TRUE)
  if (!is.null(port)) {
    args$port <- port
  }
  do.call(shiny::runApp, args)
}

shiny_app_ui <- function(input_dir, output_dir, spec_path) {
  shiny::fluidPage(
    shiny::tags$head(shiny::tags$style(shiny_app_css())),
    shiny::div(
      class = "tf-header",
      shiny::tags$h1("twinfake"),
      shiny::tags$div(class = "tf-subtitle", "Local fake-data workspace")
    ),
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::div(
          class = "tf-panel",
          shiny::tags$h2("Inputs"),
          shiny::textInput("input_dir", "Private data folder", value = input_dir),
          shiny::textInput("output_dir", "Fake output folder", value = output_dir),
          shiny::textInput("spec_path", "Spec JSON", value = spec_path),
          shiny::selectInput(
            "default_sensitivity",
            "Default sensitivity",
            choices = sensitivity_choice_labels(),
            selected = "sensitive"
          ),
          shiny::selectInput("column_ref", "Column", choices = character()),
          shiny::selectInput(
            "column_sensitivity",
            "Column action",
            choices = sensitivity_choice_labels(),
            selected = "sensitive"
          ),
          shiny::actionButton("apply_column_action", "Apply column action", icon = shiny::icon("check"), class = "tf-action"),
          shiny::numericInput("seed", "Seed", value = 20260603, min = 0, step = 1),
          shiny::checkboxInput("overwrite", "Overwrite output folder", value = TRUE),
          shiny::actionButton("scan_folder", "Scan folder", icon = shiny::icon("search"), class = "btn-primary tf-action"),
          shiny::actionButton("write_spec", "Save spec JSON", icon = shiny::icon("save"), class = "tf-action"),
          shiny::actionButton("run_fake", "Generate fake folder", icon = shiny::icon("play"), class = "tf-action")
        )
      ),
      shiny::column(
        width = 8,
        shiny::div(
          class = "tf-panel",
          shiny::tags$h2("Status"),
          shiny::uiOutput("status"),
          shiny::uiOutput("summary")
        ),
        shiny::tabsetPanel(
          id = "results_tabs",
          shiny::tabPanel("Files", DT::DTOutput("files")),
          shiny::tabPanel("Columns", DT::DTOutput("columns")),
          shiny::tabPanel("Relationships", DT::DTOutput("dependencies"))
        )
      )
    )
  )
}

shiny_app_server <- function(input_dir, output_dir, spec_path) {
  force(input_dir)
  force(output_dir)
  force(spec_path)
  function(input, output, session) {
    profile_state <- shiny::reactiveVal(NULL)
    profile_elapsed <- shiny::reactiveVal(NULL)
    column_controls <- shiny::reactiveVal(empty_column_controls())
    status <- shiny::reactiveVal("Ready. Scan the folder to load file and column summaries.")

    current_spec <- function() {
      profile <- profile_state()
      if (!is.null(profile)) {
        return(spec_from_column_controls(
          profile,
          column_controls(),
          default_sensitivity = input$default_sensitivity
        ))
      }
      if (file.exists(input$spec_path)) {
        return(read_twin_spec(input$spec_path))
      }
      NULL
    }

    shiny::observeEvent(input$input_dir, {
      profile_state(NULL)
      profile_elapsed(NULL)
      column_controls(empty_column_controls())
      update_column_choices(session, column_controls())
      status("Input folder changed. Scan the folder to refresh summaries.")
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$scan_folder, {
      start <- Sys.time()
      status("Scanning folder...")
      tryCatch(
        {
          profile <- shiny::withProgress(message = "Scanning folder", value = 0, {
            profile_folder_impl(
              input_dir = input$input_dir,
              progress = function(i, total, rel) {
                shiny::incProgress(
                  1 / max(total, 1),
                  detail = progress_detail("Profile", i, total, rel, start)
                )
              }
            )
          })
          elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
          profile_state(profile)
          profile_elapsed(elapsed)
          controls <- profile_column_controls(profile, default_sensitivity = input$default_sensitivity)
          column_controls(controls)
          update_column_choices(session, controls)
          status(paste0("Profile ready in ", format_duration(elapsed), "."))
        },
        error = function(e) {
          profile_state(NULL)
          profile_elapsed(NULL)
          column_controls(empty_column_controls())
          update_column_choices(session, column_controls())
          status(paste("Scan failed:", conditionMessage(e)))
        }
      )
    })

    shiny::observeEvent(input$default_sensitivity, {
      controls <- column_controls()
      if (!nrow(controls)) {
        return(invisible())
      }
      keep <- !controls$edited
      controls$sensitivity[keep] <- input$default_sensitivity
      column_controls(controls)
      sync_selected_column_action(session, controls, input$column_ref)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$column_ref, {
      sync_selected_column_action(session, column_controls(), input$column_ref)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$apply_column_action, {
      controls <- column_controls()
      if (!nrow(controls) || !shiny::isTruthy(input$column_ref)) {
        status("Scan the folder before applying a column action.")
        return(invisible())
      }
      idx <- which(controls$ref == input$column_ref)
      if (!length(idx)) {
        status("Selected column is no longer available. Scan the folder again.")
        return(invisible())
      }
      controls$sensitivity[[idx[[1L]]]] <- input$column_sensitivity
      controls$edited[[idx[[1L]]]] <- TRUE
      column_controls(controls)
      status(paste0(
        "Applied ", input$column_sensitivity, " to ",
        column_display_label(controls[idx[[1L]], , drop = FALSE]), "."
      ))
    })

    output$files <- DT::renderDT(
      {
        p <- profile_state()
        rows <- if (is.null(p)) empty_files_table() else profile_files_table(p)
        DT::datatable(
          rows,
          rownames = FALSE,
          filter = "top",
          options = list(pageLength = 12, scrollX = TRUE, deferRender = TRUE)
        )
      },
      server = FALSE
    )

    output$columns <- DT::renderDT(
      {
        p <- profile_state()
        rows <- if (is.null(p)) empty_columns_display_table() else profile_columns_with_controls(p, column_controls())
        DT::datatable(
          rows,
          rownames = FALSE,
          filter = "top",
          options = list(pageLength = 20, scrollX = TRUE, deferRender = TRUE)
        )
      },
      server = FALSE
    )

    output$dependencies <- DT::renderDT(
      {
        p <- profile_state()
        rows <- if (is.null(p)) empty_dependencies_table() else profile_dependencies_with_controls(p, column_controls())
        DT::datatable(
          rows,
          rownames = FALSE,
          filter = "top",
          options = list(pageLength = 20, scrollX = TRUE, deferRender = TRUE)
        )
      },
      server = FALSE
    )

    output$status <- shiny::renderUI({
      shiny::div(class = "tf-status", status())
    })

    output$summary <- shiny::renderUI({
      profile_summary_ui(profile_state(), profile_elapsed())
    })

    shiny::observeEvent(input$write_spec, {
      tryCatch(
        {
          profile <- profile_state()
          if (is.null(profile)) {
            cli_abort_twin("Scan the folder before saving a spec.")
          }
          spec <- current_spec()
          write_twin_spec(spec, input$spec_path)
          status(paste("Wrote spec:", input$spec_path))
        },
        error = function(e) {
          status(paste("Spec write failed:", conditionMessage(e)))
        }
      )
    })

    shiny::observeEvent(input$run_fake, {
      start <- Sys.time()
      status("Generating fake folder...")
      tryCatch(
        {
          result <- shiny::withProgress(message = "Generating fake folder", value = 0.1, {
            shiny::incProgress(0.2, detail = "Reading files and building key maps")
            result <- make_fake_folder(
              input_dir = input$input_dir,
              output_dir = input$output_dir,
              spec = current_spec(),
              seed = input$seed,
              overwrite = input$overwrite,
              quiet = TRUE
            )
            shiny::incProgress(0.7, detail = "Writing manifest")
            result
          })
          elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
          status(paste0(
            "Generated fake folder in ", format_duration(elapsed), ": ",
            input$output_dir,
            if (!is.null(result$manifest_path)) paste0("\nManifest: ", result$manifest_path) else ""
          ))
        },
        error = function(e) {
          status(paste("Generation failed:", conditionMessage(e)))
        }
      )
    })
  }
}

shiny_app_css <- function() {
  paste(
    "body { background: #f6f7f8; color: #1f2933; }",
    ".tf-header { margin: 18px 0 14px; }",
    ".tf-header h1 { margin: 0; font-size: 30px; font-weight: 650; letter-spacing: 0; }",
    ".tf-subtitle { color: #5b6673; font-size: 14px; margin-top: 2px; }",
    ".tf-panel { background: #fff; border: 1px solid #d9dee3; border-radius: 8px; padding: 14px; margin-bottom: 14px; }",
    ".tf-panel h2 { font-size: 17px; margin: 0 0 12px; font-weight: 650; letter-spacing: 0; }",
    ".tf-action { width: 100%; margin-top: 8px; text-align: left; }",
    ".tf-status { white-space: pre-wrap; background: #101820; color: #f8fafc; border-radius: 6px; padding: 10px 12px; min-height: 42px; }",
    ".tf-metrics { display: grid; grid-template-columns: repeat(5, minmax(0, 1fr)); gap: 8px; margin-top: 12px; }",
    ".tf-metric { border: 1px solid #d9dee3; border-radius: 6px; padding: 8px; background: #fbfcfd; }",
    ".tf-metric-label { color: #64707d; font-size: 12px; }",
    ".tf-metric-value { font-size: 20px; font-weight: 650; margin-top: 2px; }",
    ".tab-content { background: #fff; border: 1px solid #d9dee3; border-top: 0; padding: 12px; }",
    "@media (max-width: 900px) { .tf-metrics { grid-template-columns: repeat(2, minmax(0, 1fr)); } }",
    sep = "\n"
  )
}

empty_files_table <- function() {
  data.frame(
    file = character(),
    type = character(),
    status = character(),
    stringsAsFactors = FALSE
  )
}

empty_columns_table <- function() {
  data.frame(
    file = character(),
    sheet = character(),
    column = character(),
    class = character(),
    missing_prop = numeric(),
    unique_rate = numeric(),
    key_suggestion = logical(),
    stringsAsFactors = FALSE
  )
}

empty_columns_display_table <- function() {
  out <- empty_columns_table()
  out$sensitivity <- character()
  out$role <- character()
  out$custom_action <- logical()
  out[, c(
    "file", "sheet", "column", "sensitivity", "role", "custom_action",
    "class", "missing_prop", "unique_rate", "key_suggestion"
  )]
}

empty_column_controls <- function() {
  data.frame(
    ref = character(),
    file = character(),
    sheet = character(),
    column = character(),
    sensitivity = character(),
    role = character(),
    edited = logical(),
    stringsAsFactors = FALSE
  )
}

profile_column_controls <- function(profile, default_sensitivity = "sensitive") {
  columns <- profile_columns_table(profile)
  if (!nrow(columns)) {
    return(empty_column_controls())
  }
  data.frame(
    ref = as.character(seq_len(nrow(columns))),
    file = columns$file,
    sheet = columns$sheet,
    column = columns$column,
    sensitivity = sensitivity_to_default(default_sensitivity),
    role = ifelse(columns$key_suggestion, "key", ""),
    edited = FALSE,
    stringsAsFactors = FALSE
  )
}

profile_columns_with_controls <- function(profile, controls) {
  rows <- profile_columns_table(profile)
  if (!nrow(rows)) {
    return(empty_columns_display_table())
  }
  keys <- control_match_key(rows$file, rows$sheet, rows$column)
  control_keys <- control_match_key(controls$file, controls$sheet, controls$column)
  idx <- match(keys, control_keys)
  rows$sensitivity <- controls$sensitivity[idx]
  rows$role <- controls$role[idx]
  rows$custom_action <- controls$edited[idx]
  rows$sensitivity[is.na(rows$sensitivity)] <- "sensitive"
  rows$role[is.na(rows$role)] <- ""
  rows$custom_action[is.na(rows$custom_action)] <- FALSE
  rows[, c(
    "file", "sheet", "column", "sensitivity", "role", "custom_action",
    "class", "missing_prop", "unique_rate", "key_suggestion"
  )]
}

control_match_key <- function(file, sheet, column) {
  sheet <- ifelse(is.na(sheet), "", sheet)
  paste(file, sheet, column, sep = "\t")
}

update_column_choices <- function(session, controls) {
  choices <- if (!nrow(controls)) {
    character()
  } else {
    out <- controls$ref
    names(out) <- make.unique(vapply(
      seq_len(nrow(controls)),
      function(i) column_display_label(controls[i, , drop = FALSE]),
      character(1L)
    ))
    out
  }
  shiny::updateSelectInput(
    session,
    "column_ref",
    choices = choices,
    selected = if (length(choices)) choices[[1L]] else character()
  )
  shiny::updateSelectInput(
    session,
    "column_sensitivity",
    selected = if (nrow(controls)) controls$sensitivity[[1L]] else "sensitive"
  )
}

sync_selected_column_action <- function(session, controls, ref) {
  if (!nrow(controls) || !shiny::isTruthy(ref)) {
    shiny::updateSelectInput(session, "column_sensitivity", selected = "sensitive")
    return(invisible())
  }
  idx <- which(controls$ref == ref)
  if (!length(idx)) {
    return(invisible())
  }
  shiny::updateSelectInput(session, "column_sensitivity", selected = controls$sensitivity[[idx[[1L]]]])
  invisible()
}

column_display_label <- function(row) {
  sheet <- row$sheet[[1L]]
  sheet_label <- if (!is.na(sheet) && nzchar(sheet)) paste0(" [", sheet, "]") else ""
  paste0(row$file[[1L]], sheet_label, " / ", row$column[[1L]])
}

profile_files_table <- function(profile) {
  profiled <- data.frame(
    file = names(profile$files),
    type = vapply(profile$files, profile_file_type, character(1L)),
    status = "profiled",
    stringsAsFactors = FALSE
  )
  skipped <- if (length(profile$skipped)) {
    data.frame(
      file = names(profile$skipped),
      type = "unsupported",
      status = "skipped",
      stringsAsFactors = FALSE
    )
  } else {
    empty_files_table()
  }
  rbind(profiled, skipped)
}

profile_file_type <- function(x) {
  if (inherits(x, "twinfake_profile")) {
    return(x$type %||% "table")
  }
  if (is.list(x) && length(x) && all(vapply(x, inherits, logical(1L), "twinfake_profile"))) {
    return("excel")
  }
  "object"
}

profile_summary_ui <- function(profile, elapsed = NULL) {
  if (is.null(profile)) {
    return(shiny::div(
      class = "tf-metrics",
      metric_box("Profile", "Not loaded"),
      metric_box("Files", "0"),
      metric_box("Columns", "0"),
      metric_box("Relationships", "0"),
      metric_box("Elapsed", "--")
    ))
  }
  files <- profile_files_table(profile)
  columns <- profile_columns_table(profile)
  dependencies <- profile_dependencies_table(profile)
  metric_values <- list(
    metric_box("Profile", "Ready"),
    metric_box("Files", nrow(files)),
    metric_box("Columns", nrow(columns)),
    metric_box("Relationships", nrow(dependencies)),
    metric_box("Elapsed", format_duration(elapsed %||% 0))
  )
  do.call(shiny::div, c(list(class = "tf-metrics"), metric_values))
}

metric_box <- function(label, value) {
  shiny::div(
    class = "tf-metric",
    shiny::div(class = "tf-metric-label", label),
    shiny::div(class = "tf-metric-value", as.character(value))
  )
}

progress_detail <- function(label, i, total, rel, start) {
  elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  remaining <- if (i > 0 && total > i) elapsed / i * (total - i) else 0
  paste0(
    label, " ", i, "/", total,
    " | elapsed ", format_duration(elapsed),
    " | remaining ", format_duration(remaining),
    " | ", rel
  )
}

format_duration <- function(seconds) {
  seconds <- max(0, as.numeric(seconds %||% 0))
  if (!is.finite(seconds)) {
    return("--")
  }
  if (seconds < 60) {
    return(paste0(round(seconds), "s"))
  }
  minutes <- floor(seconds / 60)
  secs <- round(seconds %% 60)
  paste0(minutes, "m ", secs, "s")
}

profile_columns_table <- function(profile) {
  rows <- list()
  for (file in names(profile$files)) {
    prof <- profile$files[[file]]
    if (inherits(prof, "twinfake_profile")) {
      rows <- c(rows, profile_columns_rows(file, NA_character_, prof))
    } else if (is.list(prof)) {
      for (sheet in names(prof)) {
        if (inherits(prof[[sheet]], "twinfake_profile")) {
          rows <- c(rows, profile_columns_rows(file, sheet, prof[[sheet]]))
        }
      }
    }
  }
  if (!length(rows)) {
    return(data.frame(
      file = character(),
      sheet = character(),
      column = character(),
      class = character(),
      missing_prop = numeric(),
      unique_rate = numeric(),
      key_suggestion = logical()
    ))
  }
  do.call(rbind, rows)
}

profile_columns_rows <- function(file, sheet, prof) {
  column_names <- prof$column_names %||% names(prof$columns)
  lapply(seq_along(prof$columns), function(i) {
    col <- column_names[[i]]
    cp <- prof$columns[[i]]
    data.frame(
      file = file,
      sheet = sheet,
      column = safe_profile_column_name(col, i),
      class = paste(cp$class, collapse = "/"),
      missing_prop = round(as.numeric(cp$missing_prop %||% NA_real_), 3),
      unique_rate = round(as.numeric(cp$unique_rate %||% NA_real_), 3),
      key_suggestion = col %in% prof$key_suggestions,
      stringsAsFactors = FALSE
    )
  })
}

empty_dependencies_table <- function() {
  data.frame(
    file = character(),
    sheet = character(),
    parent = character(),
    child = character(),
    relationship = character(),
    parent_action = character(),
    child_action = character(),
    tied_when_generated = character(),
    stringsAsFactors = FALSE
  )
}

profile_dependencies_table <- function(profile) {
  rows <- list()
  for (file in names(profile$files)) {
    prof <- profile$files[[file]]
    if (inherits(prof, "twinfake_profile")) {
      rows <- c(rows, profile_dependency_rows(file, NA_character_, prof))
    } else if (is.list(prof)) {
      for (sheet in names(prof)) {
        if (inherits(prof[[sheet]], "twinfake_profile")) {
          rows <- c(rows, profile_dependency_rows(file, sheet, prof[[sheet]]))
        }
      }
    }
  }
  if (!length(rows)) {
    return(empty_dependencies_table())
  }
  do.call(rbind, rows)
}

profile_dependency_rows <- function(file, sheet, prof) {
  deps <- prof$dependencies %||% list()
  lapply(deps, function(dep) {
    data.frame(
      file = file,
      sheet = sheet,
      parent = dep$parent,
      child = dep$child,
      relationship = dependency_type_label(dep$type),
      parent_action = "",
      child_action = "",
      tied_when_generated = "",
      stringsAsFactors = FALSE
    )
  })
}

profile_dependencies_with_controls <- function(profile, controls) {
  rows <- profile_dependencies_table(profile)
  if (!nrow(rows)) {
    return(rows)
  }
  control_keys <- control_match_key(controls$file, controls$sheet, controls$column)
  parent_idx <- match(control_match_key(rows$file, rows$sheet, rows$parent), control_keys)
  child_idx <- match(control_match_key(rows$file, rows$sheet, rows$child), control_keys)
  rows$parent_action <- controls$sensitivity[parent_idx]
  rows$child_action <- controls$sensitivity[child_idx]
  rows$parent_action[is.na(rows$parent_action)] <- "sensitive"
  rows$child_action[is.na(rows$child_action)] <- "sensitive"
  tied <- !is_original_value_action(rows$parent_action) &
    !(rows$child_action %in% dependency_child_action_overrides())
  rows$tied_when_generated <- ifelse(tied, "yes", "overridden")
  rows
}

dependency_type_label <- function(type) {
  labels <- c(
    duplicate = "duplicate values",
    lower = "lower-case transform",
    upper = "upper-case transform",
    trim = "trimmed text",
    prefix = "prefix",
    suffix = "suffix",
    categorical_map = "perfect categorical map",
    date_year = "date year",
    date_month = "date month",
    date_day = "date day",
    round = "rounded numeric"
  )
  unname(labels[[type]] %||% type)
}

skeleton_spec_from_profile <- function(profile, default_sensitivity = "sensitive") {
  files <- list()
  for (file in names(profile$files)) {
    prof <- profile$files[[file]]
    if (inherits(prof, "twinfake_profile")) {
      files[[file]] <- list(columns = skeleton_columns(prof, default_sensitivity))
    } else if (is.list(prof)) {
      for (sheet in names(prof)) {
        if (inherits(prof[[sheet]], "twinfake_profile")) {
          files[[paste0(file, ":", sheet)]] <- list(columns = skeleton_columns(prof[[sheet]], default_sensitivity))
        }
      }
    }
  }
  list(
    defaults = list(
      sensitivity = default_sensitivity,
      engine = "pipeline",
      risk_level = "strict"
    ),
    files = files,
    keys = list()
  )
}

spec_from_column_controls <- function(profile, controls, default_sensitivity = "sensitive") {
  if (!nrow(controls)) {
    return(skeleton_spec_from_profile(profile, default_sensitivity = default_sensitivity))
  }
  files <- list()
  for (i in seq_len(nrow(controls))) {
    file_key <- profile_spec_key(controls$file[[i]], controls$sheet[[i]])
    files[[file_key]] <- files[[file_key]] %||% list(columns = list())
    column_spec <- list(sensitivity = controls$sensitivity[[i]])
    if (nzchar(controls$role[[i]] %||% "")) {
      column_spec$role <- controls$role[[i]]
    }
    files[[file_key]]$columns[[controls$column[[i]]]] <- column_spec
  }
  list(
    defaults = list(
      sensitivity = sensitivity_to_default(default_sensitivity),
      engine = "pipeline",
      risk_level = "strict"
    ),
    files = files,
    keys = list()
  )
}

profile_spec_key <- function(file, sheet) {
  if (is.na(sheet) || !nzchar(sheet)) {
    return(file)
  }
  paste0(file, ":", sheet)
}

skeleton_columns <- function(prof, default_sensitivity) {
  out <- list()
  column_names <- prof$column_names %||% names(prof$columns)
  for (i in seq_along(prof$columns)) {
    col <- column_names[[i]]
    out[[safe_profile_column_name(col, i)]] <- list(
      sensitivity = default_sensitivity,
      role = if (col %in% prof$key_suggestions) "key" else NULL
    )
  }
  out
}
