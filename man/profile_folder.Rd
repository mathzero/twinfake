\name{profile_folder}
\alias{profile_folder}
\title{Profile a folder of data files}
\usage{
profile_folder(input_dir, recursive = TRUE, include = NULL, exclude = NULL,
  sensitivity = "all", public_codes = NULL, key_cols = NULL,
  risk_level = c("strict", "balanced", "utility"))
}
\arguments{
\item{input_dir}{Folder to profile.}
\item{recursive}{Recurse into subdirectories.}
\item{include, exclude}{Optional regex filters for relative paths.}
\item{sensitivity}{Default sensitivity.}
\item{public_codes}{Optional columns whose labels may be preserved.}
\item{key_cols}{Optional key-column hints.}
\item{risk_level}{Privacy/utility risk level.}
}
\value{A \code{twinfake_folder_profile} object.}
\description{
Scans supported files and returns privacy-safe structure, schema, missingness,
dirty-pattern, and dependency summaries.
}
