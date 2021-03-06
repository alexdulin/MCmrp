% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/wtd-tab.R
\name{wtd_tab}
\alias{wtd_tab}
\title{Create weighted frequency table with multiple row variables}
\usage{
wtd_tab(data, xvars, yvar = NULL, weights = NULL,
  normwt = getOption("MC.normwt"), nowt.show = getOption("MC.nowt.show"),
  dem.table = FALSE, top.table = FALSE)
}
\arguments{
\item{data}{a data frame containing the variables to use in the table.}

\item{xvars}{character: vector of variable names in data for x-axis rows.}

\item{yvar}{character: response variable for the y-axis in a two-way table.}

\item{weights}{character: column name in data containing the sample weights.}

\item{normwt}{logical: if TRUE, normalize weights so that the total weighted count is the
same as the unweighted one. Defaults to \code{options(MC.normwt)}.}

\item{nowt.show}{logical: if TRUE, include unweighted registered voter statistics at the end
of the table. Defaults to \code{options(MC.nowt.show)}.}

\item{dem.table}{logical: if TRUE, creates data for a demographic summary table.}

\item{top.table}{logical: if TRUE, creates data for a topline table.}
}
\value{
If \code{yvar} is not provided, returns a weighted one-way frequency table of \code{xvars}. Otherwise,
       returns a weighted two-way frequency table of \code{xvars} and \code{yvar}.
}
\description{
Generate combined weighted frequency tables for multiple x-axis variables,
both for oneway and two-way tables.
}
\details{
An updated version of the \code{\link[questionr]{wtd_table}} function from the questionr package with the added
functionality of taking multiple vectors as the x argument. Additionally produces rownames
for the resulting table using the levels of the variables passed to \code{xvars} and column names
using the levels of the variable passed to \code{yvar}.
}
\examples{
\donttest{
# Regular two-way frequency table
wtab1 <- wtd_tab(data, listVars, vars[3], 'wts')
wtab1

# Demographic frequency table
wtab2 <- wtd_tab(data, listVars, NULL, 'wts', dem.table = TRUE)
wtab2

# Topline frequency table
wtab3 <- wtd_tab(data, vars, 'xdemAll', 'wts', top.table = TRUE)
wtab3
}
}
\seealso{
\code{\link[questionr]{wtd_table}}
}

