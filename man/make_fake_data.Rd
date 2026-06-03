\name{make_fake_data}
\alias{make_fake_data}
\title{Create a privacy-preserving fake version of a data object}
\usage{
make_fake_data(x, spec = NULL, seed = NULL, sensitivity = "all",
  preserve_row_count = TRUE,
  engine = c("pipeline", "synthpop", "independent"),
  risk_level = c("strict", "balanced", "utility"), quiet = FALSE)
}
\arguments{
\item{x}{A data frame or supported R object.}
\item{spec}{Optional twinfake spec list.}
\item{seed}{Optional numeric seed.}
\item{sensitivity}{Default sensitivity.}
\item{preserve_row_count}{Preserve the input row count.}
\item{engine}{Generation engine.}
\item{risk_level}{Privacy/utility risk level.}
\item{quiet}{Suppress informational messages.}
}
\value{A fake object matching the broad structure of \code{x}.}
\description{
Creates fake data preserving schema, missingness, dirty-value patterns,
duplicate structure, and simple deterministic relationships. All columns are
sensitive by default.
}
