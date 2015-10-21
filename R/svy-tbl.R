#' Create a table from a survey data frame
#'
#' Quick and simple methods for creating a weighted frequency table for columns
#' in a \code{svy_df} object. Printing the table will return formatted output
#' with the question text of \code{x}.
#'
#' @param data A survey data frame created by \code{\link{svy_df}}.
#' @param x,y character: Column names in \code{data} to tabualte. If \code{y} is
#'   not \code{NULL}, creates a two-way frequency table.
#' @param w character: Optional column name containing a vector of survey weights.
#' @param prop logical: If \code{TRUE}, print proportions instead of counts.
#' @param digits numeric: Number of digits to round counts to.
#' @return A (weighted) frequency table of class \code{svy_tbl}.
#'
#' @examples
#' \donttest{
#' ## Make sure `data` is a svy_df
#' is.svy_df(data)
#'
#' ## Make a one-way frequency table
#' my.tab1 <- table(data, "nr2", w = "wts")
#'
#' ## Make a table of two variables
#' my.tab2 <- table(data, "nr2", "demGender", "wts")
#'
#' ## Print formatted output
#' my.tab2
#' }
#'
#' @name svy_tbl
NULL


#' @export
table <- function(x, ...) UseMethod('table')

#' @export
table.default <- function (...) {
  base::table(...)
}


#' @export
#' @rdname svy_tbl
table.svy_df <- function(data, x, y = NULL, w = NULL, prop = FALSE, digits = 0) {
  xvar <- x
  x <- .subset2(data, x)

  if (!is.svy_df(data))
    return(table(x))

  lx <- levels(x)
  nlx <- nlevels(x)
  nux <- length(sort(unique(x)))
  qtx <- qtext(x)

  if (nux == nlx)
    tabn <- lx
  else
    tabn <- sort(unique(x))
  tabn <- c(tabn, 'N')
  tabd <- nux + 1

  if (!is.null(w))
    w <- .subset2(data, w)
  else
    w <- rep(1, nrow(data))
  if (length(w) != length(x))
    stop("columns 'x' and 'w' must be the same length in 'data'")
  if (!is.null(y)) {
    yvar <- y
    y <- .subset2(data, y)
    if (length(y) != length(x))
      stop("columns 'x' and 'y' must be the same length in 'data'")
    if (length(w) != length(y))
      stop("columns 'y' and 'w' must be the same length in 'data'")
    ly <- levels(y)
    nly <- nlevels(y)
    nuy <- length(sort(unique(y)))
    qty <- qtext(y)
    if (nuy == nly)
      tabn <- list(ly, tabn)
    else
      tabn <- list(sort(unique(y)), tabn)
    tabd <- c(nuy, tabd)
    ind <- list(y, x)
  } else {
    ind <- x
    tabn <- list('', tabn)
    tabd <- list(1, tabd)
  }
  tab <- tapply(w, ind, sum)
  if (length(dim(tab)) > 1) {
    tn <- rowSums(tab)
    if (prop)
      tab <- tab / tn
    tab <- cbind(tab, tn)
  } else {
    tn <- sum(tab)
    if (prop)
      tab <- tab / tn
    tab <- c(tab, tn)
  }
  tab <- array(tab, dim = tabd, dimnames = tabn)
  if (prop)
    digits <- 2
  tab <- round(tab, digits)
  attr(tab, "xvar") <- xvar
  attr(tab, "qx") <- qtx
  class(tab) <- c("svy_tbl", class(tab))
  tab
}




#' @export
print.svy_tbl <- function(x) {
  qx <- attr(x, "qx")
  qx <- paste0(strwrap(qx, width = 80), collapse = "\n")
  xvar <- attr(x, "xvar")

  attr(x, "xvar") <- NULL
  attr(x, "qx") <- NULL
  class(x) <- class(x)[-1]
  cat("\n*** Question Text ", "(", xvar, ")\n", sep = "")
  cat(qx, "\n\n")
  cat("--------------------------------------------------\n")
  print(x)
  cat("\n")
}
