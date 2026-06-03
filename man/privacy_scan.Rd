\name{privacy_scan}
\alias{privacy_scan}
\title{Scan fake outputs for possible sensitive literal leaks}
\usage{privacy_scan(output_dir, forbidden_values = NULL, real_data = NULL)}
\arguments{
\item{output_dir}{Folder or file to scan.}
\item{forbidden_values}{Optional forbidden literal strings.}
\item{real_data}{Optional real data object used in memory to derive high-risk tokens.}
}
\value{A data frame with \code{file}, \code{column}, \code{token_type}, and \code{severity}.}
\description{
Searches generated files and reports for forbidden literal strings supplied by
the user and high-risk tokens derived from \code{real_data}. Tokens are not
written to disk or returned in the result.
}
