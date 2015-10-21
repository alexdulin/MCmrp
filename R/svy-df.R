#' Create a survey data frame
#'
#' A survey data frame that employs the same generic methods as a regular
#' \code{data.frame} with additional functionality for preserving levels and
#' question text attributes for columns.
#'
#' Each column of a \code{svy_df} generally represents a question from a opinion
#' survey, where the values represent unique response choices, variable levels
#' represent the response choice text, and the question text represents the full
#' wording of the survey item as displayed to the respondent. Each row
#' represents a unique respondent.
#'
#' When subsetting a \code{svy_df} object using standard indexing methods (i.e.
#' \code{'['}), variable levels and question text will be copied from the
#' original object to the subsetted object using \code{\link{copy_lev}}.
#'
#' @param data a data frame
#' @param questions a data frame with a minimum of two columns, where the first
#'   column contains matching names of columns in \code{data} and the second
#'   column contains the associated full question text.
#' @param x An object to test or print.
#' @param n,width,... Arguemnts passed to \code{print}.
#' @return A object of class \code{svy_df} with optional attributes for question
#'   text and variable levels.
#'
#' @name svy_df
NULL


#' @export
#' @rdname svy_df
svy_df <- function(data, questions = NULL) {
  if (!is.null(questions)) {
    if (!is.data.frame(questions))
      stop("'questions' must be an object of class data.frame")
    qid <- questions[, 1]
    qtxt <- questions[, 2]
    if (!is.character(qid) | !is.character(qtxt))
      stop(paste("The first two columns of 'questions' must be character vectors",
                 "containing question names and full text, respectively"))
    pos <- qid %in% names(data)
    if (length(pos)) {
      qid <- qid[pos]
      qtxt <- qtxt[pos]
      for (i in seq_along(qid))
        attr(data[[qid[i]]], 'qtext') <- qtxt[i]
    }
  }
  qnull <- which(sapply(data, function(x) is.null(qtext(x))))
  if (length(qnull))
    for (i in qnull)
      qtext(data[[i]]) <- ''

  class(data) <- c('svy_df', 'data.frame')
  data
}



#' @export
#' @rdname svy_df
is.svy_df <- function(x) {
  is(x, 'svy_df')
}


#' @export
#' @rdname svy_df
print.svy_df <- function(x, ..., n = NULL, width = NULL) {
  cat("Source: survey data frame ", dplyr::dim_desc(x), "\n", sep = "")
  cat("\n")
  print(dplyr::trunc_mat(x, n = n, width = width))

  invisible(x)
}



#' @export
`[.svy_df` <- function (x, i, j,
                        drop = if (missing(i)) TRUE else length(cols) == 1)
{
  x0 <- x
  mdrop <- missing(drop)
  Narg <- nargs() - (!mdrop)
  has.j <- !missing(j)
  if (!all(names(sys.call()) %in% c("", "drop")) && !isS4(x))
    warning("named arguments other than 'drop' are discouraged")
  if (Narg < 3L) {
    if (!mdrop)
      warning("'drop' argument will be ignored")
    if (missing(i))
      return(x)
    if (is.matrix(i))
      return(as.matrix(x)[i])
    nm <- names(x)
    if (is.null(nm))
      nm <- character()
    if (!is.character(i) && anyNA(nm)) {
      names(nm) <- names(x) <- seq_along(x)
      y <- NextMethod("[")
      cols <- names(y)
      if (anyNA(cols))
        stop("undefined columns selected")
      cols <- names(y) <- nm[cols]
    }
    else {
      y <- NextMethod("[")
      cols <- names(y)
      if (!is.null(cols) && anyNA(cols))
        stop("undefined columns selected")
    }
    if (anyDuplicated(cols))
      names(y) <- make.unique(cols)
    attr(y, "row.names") <- .row_names_info(x, 0L)
    attr(y, "class") <- oldClass(x)
    return(y)
  }
  if (missing(i)) {
    if (drop && !has.j && length(x) == 1L)
      return(.subset2(x, 1L))
    nm <- names(x)
    if (is.null(nm))
      nm <- character()
    if (has.j && !is.character(j) && anyNA(nm)) {
      names(nm) <- names(x) <- seq_along(x)
      y <- .subset(x, j)
      cols <- names(y)
      if (anyNA(cols))
        stop("undefined columns selected")
      cols <- names(y) <- nm[cols]
    }
    else {
      y <- if (has.j)
        .subset(x, j)
      else x
      cols <- names(y)
      if (anyNA(cols))
        stop("undefined columns selected")
    }
    if (drop && length(y) == 1L)
      return(.subset2(y, 1L))
    if (anyDuplicated(cols))
      names(y) <- make.unique(cols)
    nrow <- .row_names_info(x, 2L)
    if (drop && !mdrop && nrow == 1L)
      return(structure(y, class = NULL, row.names = NULL))
    else {
      attr(y, "class") <- oldClass(x)
      attr(y, "row.names") <- .row_names_info(x, 0L)
      return(y)
    }
  }
  xx <- x
  cols <- names(xx)
  x <- vector("list", length(x))
  x <- .Internal(copyDFattr(xx, x))
  oldClass(x) <- attr(x, "row.names") <- NULL
  if (has.j) {
    nm <- names(x)
    if (is.null(nm))
      nm <- character()
    if (!is.character(j) && anyNA(nm))
      names(nm) <- names(x) <- seq_along(x)
    x <- x[j]
    cols <- names(x)
    if (drop && length(x) == 1L) {
      if (is.character(i)) {
        rows <- attr(xx, "row.names")
        i <- pmatch(i, rows, duplicates.ok = TRUE)
      }
      xj <- .subset2(.subset(xx, j), 1L)
      return(if (length(dim(xj)) != 2L) xj[i] else xj[i,
                                                      , drop = FALSE])
    }
    if (anyNA(cols))
      stop("undefined columns selected")
    if (!is.null(names(nm)))
      cols <- names(x) <- nm[cols]
    nxx <- structure(seq_along(xx), names = names(xx))
    sxx <- match(nxx[j], seq_along(xx))
  }
  else sxx <- seq_along(x)
  rows <- NULL
  if (is.character(i)) {
    rows <- attr(xx, "row.names")
    i <- pmatch(i, rows, duplicates.ok = TRUE)
  }
  for (j in seq_along(x)) {
    xj <- xx[[sxx[j]]]
    x[[j]] <- if (length(dim(xj)) != 2L)
      xj[i]
    else xj[i, , drop = FALSE]
  }
  if (drop) {
    n <- length(x)
    if (n == 1L)
      return(x[[1L]])
    if (n > 1L) {
      xj <- x[[1L]]
      nrow <- if (length(dim(xj)) == 2L)
        dim(xj)[1L]
      else length(xj)
      drop <- !mdrop && nrow == 1L
    }
    else drop <- FALSE
  }
  if (!drop) {
    if (is.null(rows))
      rows <- attr(xx, "row.names")
    rows <- rows[i]
    if ((ina <- anyNA(rows)) | (dup <- anyDuplicated(rows))) {
      if (!dup && is.character(rows))
        dup <- "NA" %in% rows
      if (ina)
        rows[is.na(rows)] <- "NA"
      if (dup)
        rows <- make.unique(as.character(rows))
    }
    if (has.j && anyDuplicated(nm <- names(x)))
      names(x) <- make.unique(nm)
    if (is.null(rows))
      rows <- attr(xx, "row.names")[i]
    attr(x, "row.names") <- rows
    oldClass(x) <- oldClass(xx)
  }
  if (!is.null(ncol(x)))
    x <- copy_lev(x, x0)
  x
}
