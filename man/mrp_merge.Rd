% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/mrp-merge.R
\name{mrp_merge}
\alias{mrp_grp}
\alias{mrp_merge}
\title{Merge cell weight predictions from a MRP model with survey data}
\usage{
mrp_merge(data, mrp.fit, by = "grp", ...)

mrp_grp(data, model)
}
\arguments{
\item{data}{A data frame containing the original survey data to merge with
the predicted cell weights.}

\item{mrp.fit}{An object of class \code{mrp.model} produced by \code{mrp_model}
to extract cell predictions from.}

\item{by}{character: The column name to merge by. Defaults to 'grp', which is
the default variable for indexing cell interactions produced by
\code{mrp_table}.}

\item{...}{Other agruments passed on to \code{\link[base]{merge}}.}
}
\value{
For \code{mrp_merge}, a data frame with merged MRP cell predictions.

        For \code{mrp_grp}, a character vector containing the various combinations
        of values from the variables used in the underlying formula passed to
        \code{model} in \code{mrp_table}.
}
\description{
Extracts the weighted and unweighted cell predictions from the output of
\code{mrp_model} and merges it with a data frame. The data frame passed to
\code{data} should be the same object used in the call to \code{mrp_model}.
For \code{mrp_grp}, adds the 'grp' column to the survey data for merging.
}
\details{
The resulting data frame will have value labels copied from the original data
object using \code{\link{copy_lev}}.
}
\seealso{
\code{\link{mrp_table}}, \code{\link{mrp_model}}
}

