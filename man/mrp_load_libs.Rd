% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/mrp-load-libs.R
\name{mrp_load_libs}
\alias{mrp_load_libs}
\title{Quickly and quietly load required libraries for MRP}
\usage{
mrp_load_libs(install = FALSE, quiet = TRUE)
}
\arguments{
\item{install}{logical: if \code{TRUE} installs all required packages.
Default is \code{FALSE}}

\item{quiet}{logical: if \code{TRUE} suppresses all package startup messages.}
}
\description{
Loads all required libraries necessary for conducting multilevel regression
and poststratification analysis.
}
\examples{
\donttest{
## Load libraries
mrp_load_libs()
## with startup messages
mrp_load_libs(quiet = FALSE)
## and install packages
mrp_load_libs(install = TRUE)
}
}

