#' Create individual-level marginal distribution table for MRP
#'
#' Creates a data frame object of class \code{mrp.table} that contains counts
#' and proportions for the interactions between all demographic variables
#' specified by \code{model}. The resulting output is designed (and required)
#' for use in estimating cell predictions using the \code{\link{mrp_model}}
#' function.
#'
#' If \code{data} is not provided, then the function will use a preset data file
#' created from the CPS Nov. 2012 voter survey.
#'
#' The value passed to \code{model} must be a list object of column names in
#' \code{data}, and each must also be passed as a formula. For example, to
#' create a MRP table for the interaction between columns named `state' and
#' `sex', the argument to \code{model} would be: \code{list(~state, ~sex)}.
#' Additionally, the first name in this list will be used to group final
#' proportions by.
#'
#' @seealso \code{\link{mrp_model}}, \code{\link{mrp_merge}}
#' @param model formula: A list object containing column names in
#'   \code{pop_data} as separate formulae defining the model to use when
#'   creating the MRP table. See Details.
#' @param data A \code{R} data frame containing population data, or a file path
#'   to read in data. If \code{NULL}, uses an internal dataset. If a file path
#'   is used, the file type must be one of \code{.rds} or \code{.csv}.
#' @param weights numeric: Optional vector of weights to use when calculating
#'   counts and proportions.
#' @return A data frame containing counts and proportions for all unique
#'   interactions between the variables specified in \code{model}.
#'
#' @export
#' @examples
#' \donttest{
#' ## Create a five way interaction table, where the final proportions will be
#' ## grouped by states.
#' model <- list(~state, ~sex, ~race4, ~hisp, ~inc3)
#'
#' x <- mrp_table(model = model)
#' }
mrp_table <- function (model, data = NULL, weights = NULL)
{
  if (is.null(data)) {
    load(system.file("Weights", "cps_nov12_clean.Rda", package = "MCmrp"))
    pop <- cps_nov12_clean
    pop[, 'w'] <- pop$wts
  } else {
    if (is.data.frame(data)) {
      pop <- data
    } else {
      if (is.character(data)) {
        if (!file.exists(data))
          stop("data must be a valid file path or data frame.")
        nc <- nchar(data)
        ftype <- tolower(substr(data, (nc - 2), nc))
        if (!(ftype %in% c('rds', 'csv'))) {
          stop("data file paths can only lead to .rds or .csv files.")
        } else {
          if (ftype == 'rds')
            pop <- readRDS(data)
          if (ftype == 'csv')
            pop <- read.csv(data, stringsAsFactors = FALSE)
        }
      }
    }
    if (is.null(weights)) {
      pop[, 'w'] <- 1
    } else {
      if (length(weights) != nrow(pop))
        stop("weights must be the same length as data")
      pop[, 'w'] <- weights
    }
  }
  if (!is.list(model))
    stop("model must be a list of column names.")
  if (any(!sapply(model, plyr::is.formula)))
    stop("All elements of model must be formulae.")

  pop <- pop[, c(as.character(substr(model, 2, nchar(model))), 'w')]
  for (i in 1:ncol(pop))
    pop[, i] <- as.numeric(pop[, i])

  res <- dplyr::group_by_(pop, .dots = model) %>%
    dplyr::tally(wt = w, sort = TRUE) %>%
    dplyr::group_by_(.dots = model[[1]]) %>%
    dplyr::mutate(prop = n / sum(n)) %>%
    dplyr::arrange_(.dots = model)
  res <- as.data.frame(res)
  res$grp <- mrp_grp(res, model)
  class(res) <- c("mrp.table", class(res))
  res
}