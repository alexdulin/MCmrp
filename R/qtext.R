#' Get and Set Variable Question Text
#'
#' Generic method for retreiving and assinging the question text associated with
#' a column in a \code{svy_df} object associated with a survey question.
#'
#' @param x a single column in a \code{svy_df} object, or a survey data frame of
#'   class \code{svy_df} to return all variable question text for.
#' @param value character: A string containing the full wording of a survey item
#'   to assign to the question text attribute of \code{x}.
#' @return An object identical to \code{x} with the 'qtext' attribute containing
#'   question text, or for \code{questions}, a two column data frame with the
#'   column names of the data in the first column and the associated question
#'   text of each in the second.
#'
#' @examples
#' \dontrun{
#' ## Show the current question text for a column
#' qtext(data$nr1)
#'
#' ## Assign the question text to a variable
#' qtext(data$nr1) <- 'yolo
#' }
#'
#' @name Question-Text
NULL

#' @export
#' @rdname Question-Text
qtext <- function(x) {
  if (is.svy_df(x)) {
    if (ncol(x) > 1) {
      qtxt <- sapply(x, qtext)
      pos <- which(sapply(qtxt, is.null))
      if (length(pos))
        qtxt[pos] <- ''
      if (is.list(qtxt))
        qtxt <- unlist(qtxt)
      res <- data.frame(qid = names(qtxt), qidFull = qtxt,
                        stringsAsFactors = FALSE,
                        row.names = 1:length(qtxt))
      return(res)
    } else {
      return(attr(x[[1]], 'qtext'))
    }
  }
  attr(x, 'qtext')
}


#' @export
#' @rdname Question-Text
"qtext<-" <- function(x, value) {
  attr(x, 'qtext') <- value
  x
}
