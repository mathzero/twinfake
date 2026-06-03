\name{make_fake_folder}
\alias{make_fake_folder}
\title{Create a fake mirrored folder tree}
\usage{
make_fake_folder(input_dir, output_dir, spec = NULL, seed = NULL,
  overwrite = FALSE, recursive = TRUE, include = NULL, exclude = NULL,
  sensitivity = "all", preserve_file_names = TRUE,
  preserve_row_count = TRUE, xlsx_sheets = "all",
  unknown = c("skip", "empty_placeholder", "copy"),
  engine = c("pipeline", "synthpop", "independent"),
  risk_level = c("strict", "balanced", "utility"), report = TRUE,
  quiet = FALSE)
}
\arguments{
\item{input_dir}{Folder containing private source files.}
\item{output_dir}{Folder to create fake files in.}
\item{spec}{Optional spec list or path to a spec JSON file.}
\item{seed}{Optional numeric seed.}
\item{overwrite}{Overwrite existing output files or a non-empty output folder.}
\item{recursive}{Recurse into subdirectories.}
\item{include, exclude}{Optional regex filters for relative paths.}
\item{sensitivity}{Default sensitivity; \code{"all"} treats all columns as sensitive.}
\item{preserve_file_names}{Preserve original relative file names.}
\item{preserve_row_count}{Preserve row counts.}
\item{xlsx_sheets}{\code{"all"} or a character vector of sheet names.}
\item{unknown}{How to handle unknown files.}
\item{engine}{Generation engine.}
\item{risk_level}{Privacy/utility risk level.}
\item{report}{Write \code{twinfake_manifest.json}.}
\item{quiet}{Suppress informational messages.}
}
\value{A \code{twinfake_folder_result} object.}
\description{
Scans supported files in \code{input_dir}, generates privacy-first fake
equivalents, and writes a parallel folder tree under \code{output_dir}. Raw real
values and real-to-fake key maps are kept in memory only.
}
