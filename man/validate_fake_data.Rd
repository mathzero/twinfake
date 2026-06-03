\name{validate_fake_data}
\alias{validate_fake_data}
\title{Validate fake data against real data structure}
\usage{validate_fake_data(real, fake, spec = NULL)}
\arguments{
\item{real}{Original data frame.}
\item{fake}{Fake data frame.}
\item{spec}{Optional twinfake spec.}
}
\value{A \code{twinfake_validation} object.}
\description{
Compares real and fake data for schema compatibility, class preservation,
missingness, dirty-pattern similarity, broad distribution similarity,
relationship preservation, and possible privacy issues.
}
