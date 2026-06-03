\name{launch_twin_app}
\alias{launch_twin_app}
\title{Launch the local twinfake Shiny app}
\usage{
launch_twin_app(input_dir, output_dir = NULL, spec_path = NULL,
  host = "127.0.0.1", port = NULL)
}
\arguments{
\item{input_dir}{Input folder to inspect.}
\item{output_dir}{Optional output folder for fake files.}
\item{spec_path}{Optional JSON spec path to load or write.}
\item{host}{Host interface. Must be local.}
\item{port}{Optional port.}
}
\value{The value returned by \code{shiny::runApp()}.}
\description{
Starts a local-only Shiny app for reviewing safe column summaries, key
suggestions, and sensitivity controls. Raw values are not displayed by default.
}
