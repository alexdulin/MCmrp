% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/mrp-table.R
\name{mrp_table}
\alias{mrp_table}
\title{Create individual-level marginal distribution table for MRP}
\usage{
mrp_table(model, data = NULL, weights = NULL)
}
\arguments{
\item{model}{formula: A list object containing column names in
\code{pop_data} as separate formulae defining the model to use when
creating the MRP table. See Details.}

\item{data}{A \code{R} data frame containing population data, or a file path
to read in data. If \code{NULL}, uses an internal dataset. If a file path
is used, the file type must be one of \code{.rds} or \code{.csv}.}

\item{weights}{numeric: Optional vector of weights to use when calculating
counts and proportions.}
}
\value{
A data frame containing counts and proportions for all unique
  interactions between the variables specified in \code{model}.
}
\description{
Creates a data frame object of class \code{mrp.table} that contains counts
and proportions for the interactions between all demographic variables
specified by \code{model}. The resulting output is designed (and required)
for use in estimating cell predictions using the \code{\link{mrp_model}}
function.
}
\details{
If \code{data} is not provided, then the function will use a preset data file
created from the CPS Nov. 2012 voter survey.

The value passed to \code{model} must be a list object of column names in
\code{data}, and each must also be passed as a formula. For example, to
create a MRP table for the interaction between columns named `state' and
`sex', the argument to \code{model} would be: \code{list(~state, ~sex)}.
Additionally, the first name in this list will be used to group final
proportions by.
}
\examples{
\donttest{
## Create a five way interaction table, where the final proportions will be
## grouped by states.
model <- list(~state, ~sex, ~race4, ~hisp, ~inc3)

x <- mrp_table(model = model)
}
}
\seealso{
\code{\link{mrp_model}}, \code{\link{mrp_merge}}
}

