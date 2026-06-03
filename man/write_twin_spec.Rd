\name{write_twin_spec}
\alias{write_twin_spec}
\title{Write a twinfake sensitivity specification}
\usage{write_twin_spec(spec, path)}
\arguments{
\item{spec}{A list describing defaults, files, columns, keys, and controls.}
\item{path}{Output JSON path.}
}
\value{Invisibly returns \code{path}.}
\description{
Writes a JSON-serialisable twinfake specification to disk. Specifications
should contain column roles and sensitivity classes, not real data values.
}
