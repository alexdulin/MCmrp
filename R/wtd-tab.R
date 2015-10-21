#' Create weighted frequency table with multiple row variables
#'
#' Generate combined weighted frequency tables for multiple x-axis variables,
#' both for oneway and two-way tables.
#'
#' An updated version of the \code{\link[questionr]{wtd_table}} function from the questionr package with the added
#' functionality of taking multiple vectors as the x argument. Additionally produces rownames
#' for the resulting table using the levels of the variables passed to \code{xvars} and column names
#' using the levels of the variable passed to \code{yvar}.
#'
#' @seealso \code{\link[questionr]{wtd_table}}
#' @param data a data frame containing the variables to use in the table.
#' @param xvars character: vector of variable names in data for x-axis rows.
#' @param yvar character: response variable for the y-axis in a two-way table.
#' @param weights character: column name in data containing the sample weights.
#' @param normwt logical: if TRUE, normalize weights so that the total weighted count is the
#'        same as the unweighted one. Defaults to \code{options(MC.normwt)}.
#' @param nowt.show logical: if TRUE, include unweighted registered voter statistics at the end
#'        of the table. Defaults to \code{options(MC.nowt.show)}.
#' @param dem.table logical: if TRUE, creates data for a demographic summary table.
#' @param top.table logical: if TRUE, creates data for a topline table.
#' @return If \code{yvar} is not provided, returns a weighted one-way frequency table of \code{xvars}. Otherwise,
#'        returns a weighted two-way frequency table of \code{xvars} and \code{yvar}.
#'
#' @examples
#' \donttest{
#' # Regular two-way frequency table
#' wtab1 <- wtd_tab(data, listVars, vars[3], 'wts')
#' wtab1
#'
#' # Demographic frequency table
#' wtab2 <- wtd_tab(data, listVars, NULL, 'wts', dem.table = TRUE)
#' wtab2
#'
#' # Topline frequency table
#' wtab3 <- wtd_tab(data, vars, 'xdemAll', 'wts', top.table = TRUE)
#' wtab3
#' }
#' @name wtd_tab
NULL

#' @export
#' @rdname wtd_tab
wtd_tab <- function (data, xvars, yvar = NULL, weights = NULL,
                     normwt = getOption("MC.normwt"), nowt.show = getOption("MC.nowt.show"),
                     dem.table = FALSE, top.table = FALSE)
{
  if (dem.table & top.table)
    stop("Error: dem.table and top.table cannot both be TRUE")
  if (top.table & !exists('range2')) {
    range2 <- rep(NA, length(xvars))
    for (i in seq_along(xvars))
      range2[i] <- grep(paste0("^", xvars[i], "$"), names(data))
  }
  if (!is.null(yvar))
    y <- .subset2(data, yvar)
  else {
    if (top.table)
      y <- .subset2(data, 'xdemAll')
    else
      y <- NULL
  }
  if (!is.null(weights))
    weights <- .subset2(data, weights)
  else
    weights <- rep(1, nrow(data))
  if (dem.table) {
    x <- .subset2(data, xvars[1])
    if (normwt)
      weights <- weights * length(x) / sum(weights)
    n <- as.numeric(.tapply2(weights, x, sum, simplify = TRUE))
  }
  for (i in seq_along(xvars)) {
    x <- .subset2(data, xvars[i])
    if (length(x) != length(weights))
      stop("x and weights lengths must be the same")
    if (!is.null(y) & (length(x) != length(y)))
      stop("x and y lengths must be the same")
    if (normwt)
      weights <- weights * length(x) / sum(weights)
    if (is.null(y))
      a <- .tapply2(weights, x, sum, simplify = TRUE)
    else
      a <- .tapply2(weights, list(x, y), sum, simplify = TRUE)
    a[is.na(a)] <- 0
    if (dem.table) {
      if (length(a) == 1)
        a <- cbind(a, a / n * 100)
      else
        a <- cbind(c(a, sum(a)), c(a / n * 100, NA))
    }
    if (top.table)
      a <- cbind(rbind(sum(a), a), c(NA, a / sum(a) * 100))
    if (i == 1)
      b <- a
    else
      b <- rbind(b, a)
  }
  if(nowt.show & !top.table & !dem.table) {
    x <- .subset2(data, xvars[1])
    weights <- .subset2(data, 'nowts')
    if (normwt)
      weights <- weights * length(x) / sum(weights)
    b <- rbind(b, .tapply2(weights, list(x, y), sum, simplify = TRUE))
    rownames(b)[nrow(b)] <- "Registered Voters (Unweighted)"
  }
  if (dem.table)
    rownames(b)[rownames(b) == ""] <- "\\textit{N}"
  if (top.table)
    rownames(b)[rownames(b) == ""] <- qtext(data[, xvars])[[2]]
  as.table(b)
}



# faster tapply function --------------------------------------------------


.tapply2 <- function (X, INDEX, FUN = NULL, ..., simplify = TRUE)
{
  FUN <- if (!is.null(FUN))
    match.fun(FUN)
  if (!is.list(INDEX))
    INDEX <- list(INDEX)
  nI <- length(INDEX)
  if (!nI)
    stop("'INDEX' is of length zero")
  namelist <- vector("list", nI)
  names(namelist) <- names(INDEX)
  extent <- integer(nI)
  nx <- length(X)
  one <- 1L
  group <- rep.int(one, nx)
  ngroup <- one
  for (i in seq_along(INDEX)) {
    index <- INDEX[[i]]
    if (length(index) != nx)
      stop("arguments must have same length")
    namelist[[i]] <- levels(index)
    extent[i] <- nlevels(index)
    group <- group + ngroup * (as.integer(index) - one)
    ngroup <- ngroup * nlevels(index)
  }
  if (is.null(FUN))
    return(group)
  ans <- lapply(X = split(X, group), FUN = FUN, ...)
  index <- as.integer(names(ans))
  if (simplify && all(unlist(lapply(ans, length)) == 1L)) {
    ansmat <- array(dim = extent, dimnames = namelist)
    ans <- unlist(ans, recursive = FALSE)
  }
  else {
    ansmat <- array(vector("list", prod(extent)), dim = extent,
                    dimnames = namelist)
  }
  if (length(index)) {
    names(ans) <- NULL
    ansmat[index] <- ans
  }
  ansmat
}