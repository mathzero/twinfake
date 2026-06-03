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
    shiny::titlePanel("twinfake"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::textInput("input_dir", "Input folder", value = input_dir),
        shiny::textInput("output_dir", "Output folder", value = output_dir),
        shiny::textInput("spec_path", "Spec path", value = spec_path),
        shiny::selectInput(
          "default_sensitivity",
          "Default sensitivity",
          choices = c("sensitive", "public_code", "copy", "drop", "hash", "structure_only"),
          selected = "sensitive"
        ),
        shiny::actionButton("refresh", "Refresh profile"),
        shiny::actionButton("write_spec", "Write spec"),
        shiny::actionButton("run_fake", "Run generator")
      ),
      shiny::mainPanel(
        shiny::tags$p("Column summaries exclude raw values. Use copy and public_code only for columns you have explicitly reviewed."),
        DT::DTOutput("files"),
        DT::DTOutput("columns"),
        shiny::verbatimTextOutput("status")
      )
    )
  )
}

shiny_app_server <- function(input_dir, output_dir, spec_path) {
  force(input_dir)
  force(output_dir)
  force(spec_path)
  function(input, output, session) {
    profile <- shiny::eventReactive(input$refresh, {
      profile_folder(input$input_dir)
    }, ignoreNULL = FALSE)

    output$files <- DT::renderDT({
      p <- profile()
      rows <- data.frame(
        file = names(p$files),
        type = vapply(p$files, function(x) x$type %||% "workbook", character(1L)),
        stringsAsFactors = FALSE
      )
      DT::datatable(rows, rownames = FALSE, options = list(pageLength = 10))
    })

    output$columns <- DT::renderDT({
      p <- profile()
      rows <- profile_columns_table(p)
      DT::datatable(rows, rownames = FALSE, options = list(pageLength = 20))
    })

    status <- shiny::reactiveVal("Ready.")

    shiny::observeEvent(input$write_spec, {
      spec <- skeleton_spec_from_profile(profile(), default_sensitivity = input$default_sensitivity)
      write_twin_spec(spec, input$spec_path)
      status(paste("Wrote spec:", input$spec_path))
    })

    shiny::observeEvent(input$run_fake, {
      make_fake_folder(
        input_dir = input$input_dir,
        output_dir = input$output_dir,
        spec = if (file.exists(input$spec_path)) read_twin_spec(input$spec_path) else NULL,
        overwrite = TRUE,
        quiet = TRUE
      )
      status(paste("Generated fake folder:", input$output_dir))
    })

    output$status <- shiny::renderText(status())
  }
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
