#' Copy Survey Data Variable Levels and Question Text
#'
#' Applies levels and question text attributes to variables in a subsetted
#' \code{svy_df} object using the levels from the columns with matching names in
#' the original data frame.
#'
#' When subsetting a data frame or similar \code{R} object, variable levels (for
#' factors and non-factors) and other data attributes are not copied using any
#' common function. Rather than applying the levels for each variable by hand,
#' this function completes the process in one call by matching the names of
#' variables in the subset data fram with those in the original, and then
#' copying the matched variable levels in the original object to the new one.
#'
#' @param x the new data frame that is a subset of another data frame object.
#' @param y the original data frame containing the variable levels.
#' @return A data frame where all matched column names have variable levels.
#'
#' @examples
#' \donttest{
#' ## subset a data frame
#' x <- data[, c('nr1', 'nr2Bin', 'nr2', 'nr3')]
#' levels(x$nr1)
#' #> NULL
#'
#' ## copy the levels
#' x <- copy_lev(x, data)
#' levels(x$nr1)
#'
#' ## Do it with dplyr/chaining
#' library(dplyr)
#' x <- data[, c('nr1', 'nr2Bin', 'nr2', 'nr3')] %>% copy_lev(data)
#' levels(x$nr1)
#' }
#'
#' @name copy_lev
NULL

#' @export
#' @rdname copy_lev
copy_lev <- function(x, y)
{
  if (!is.null(ncol(x)))
    for (i in 1:ncol(x))
      qtext(x[[i]]) <- ''
  else
    qtext(x) <- ''
  z <- names(x)[names(x) %in% names(y)]
  if (length(z))
    for (i in z) {
      levels(x[[i]]) <- levels(y[[i]])
      qtext(x[[i]]) <- qtext(y[[i]])
    }
  if (is.svy_df(y) & !is.svy_df(x))
    x <- svy_df(x)
  x
}
