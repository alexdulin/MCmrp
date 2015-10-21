#' Merge cell weight predictions from a MRP model with survey data
#'
#' Extracts the weighted and unweighted cell predictions from the output of
#' \code{mrp_model} and merges it with a data frame. The data frame passed to
#' \code{data} should be the same object used in the call to \code{mrp_model}.
#' For \code{mrp_grp}, adds the 'grp' column to the survey data for merging.
#'
#' The resulting data frame will have value labels copied from the original data
#' object using \code{\link{copy_lev}}.
#'
#' @seealso \code{\link{mrp_table}}, \code{\link{mrp_model}}
#' @param data A data frame containing the original survey data to merge with
#'   the predicted cell weights.
#' @param mrp.fit An object of class \code{mrp.model} produced by \code{mrp_model}
#'   to extract cell predictions from.
#' @param by character: The column name to merge by. Defaults to 'grp', which is
#'   the default variable for indexing cell interactions produced by
#'   \code{mrp_table}.
#' @param ... Other agruments passed on to \code{\link[base]{merge}}.
#' @return For \code{mrp_merge}, a data frame with merged MRP cell predictions.
#'
#'         For \code{mrp_grp}, a character vector containing the various combinations
#'         of values from the variables used in the underlying formula passed to
#'         \code{model} in \code{mrp_table}.
#'
#' @export
#' @name mrp_merge
NULL

#' @rdname mrp_merge
mrp_merge <- function(data, mrp.fit, by = "grp", ...)
  {
  if (!is(mrp.fit, "mrp.model"))
    stop("mrp.fit must be an object of class 'mrp.model'.")
  res <- merge(data, mrp.fit$predictions, by = by, all.x = TRUE, ...)
  res$cellpred[is.na(res$cellpred)] <- 0
  res$w_cellpred[is.na(res$w_cellpred)] <- 0
  res <- copy_lev(res, data)
  res
}

#' @export
#' @rdname mrp_merge
mrp_grp <- function (data, model)
  {
  if (!is.list(model))
    stop("model must be a list of column names.")
  if (any(!sapply(model, plyr::is.formula)))
    stop("All elements of model must be formulae.")
  apply(data[, as.character(substr(model, 2, nchar(model)))], 1, paste, collapse = "_")
}