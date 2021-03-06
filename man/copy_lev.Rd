% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/copy-lev.R
\name{copy_lev}
\alias{copy_lev}
\title{Copy Survey Data Variable Levels and Question Text}
\usage{
copy_lev(x, y)
}
\arguments{
\item{x}{the new data frame that is a subset of another data frame object.}

\item{y}{the original data frame containing the variable levels.}
}
\value{
A data frame where all matched column names have variable levels.
}
\description{
Applies levels and question text attributes to variables in a subsetted
\code{svy_df} object using the levels from the columns with matching names in
the original data frame.
}
\details{
When subsetting a data frame or similar \code{R} object, variable levels (for
factors and non-factors) and other data attributes are not copied using any
common function. Rather than applying the levels for each variable by hand,
this function completes the process in one call by matching the names of
variables in the subset data fram with those in the original, and then
copying the matched variable levels in the original object to the new one.
}
\examples{
\donttest{
## subset a data frame
x <- data[, c('nr1', 'nr2Bin', 'nr2', 'nr3')]
levels(x$nr1)
#> NULL

## copy the levels
x <- copy_lev(x, data)
levels(x$nr1)

## Do it with dplyr/chaining
library(dplyr)
x <- data[, c('nr1', 'nr2Bin', 'nr2', 'nr3')] \%>\% copy_lev(data)
levels(x$nr1)
}
}

