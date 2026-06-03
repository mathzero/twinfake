\name{profile_data}
\alias{profile_data}
\title{Profile a data object for pipeline twin generation}
\usage{
profile_data(x, sensitivity = "all", public_codes = NULL, key_cols = NULL,
  deterministic = NULL, risk_level = c("strict", "balanced", "utility"))
}
\arguments{
\item{x}{A data frame or object to profile.}
\item{sensitivity}{Default sensitivity.}
\item{public_codes}{Optional columns whose labels may be preserved.}
\item{key_cols}{Optional manual key-column specification.}
\item{deterministic}{Optional deterministic relationship hints.}
\item{risk_level}{Privacy/utility risk level.}
}
\value{A \code{twinfake_profile} object.}
\description{
Profiles structure, classes, missingness, dirty patterns, broad distributions,
and simple deterministic relationships without storing raw sensitive values by
default.
}
