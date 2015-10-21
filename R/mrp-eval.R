#' Evaluate MRP cell predictions
#'
#' Compares the accuracy of the cell predictions produced by \code{mrp_model}
#' with the standard raked demographic weights in the survey data frame
#' \code{data}. The comparison is conducted by creating weighted proportion
#' tables for \code{xvars} and \code{yvar} using the two sets of weights, and
#' then generating various descriptive statistics for the two tables compared to
#' the known values of variables as found in \code{target}.
#'
#' If a data frame is passed to \code{target}, the object will be coerced into a
#' table using the first column of the data frame as the table row names. The
#' column names of \code{target} do not need to be exact matches to the variable
#' levels of \code{yvar}, but the table row names (or first column, if a data
#' frame) must be exact string matches as the levels of the variables in
#' \code{xvars}, and must be in the same order as \code{xvars}. If the order is
#' not exactly matched, then the analysis will compare the estimated values with
#' incorrect known values.
#'
#' Since the evaluation focuses on the accuracy of the different demographic
#' groups' in \code{xvars} proportions compared to the known values, all of the
#' variables specified in \code{xvars} within \code{data} need to have matching
#' value labels (levels) as those in \code{target}, as these will become the
#' rows (and row names) of the weighted crosstabulations created by
#' \code{wtd_tab}. Any demographic groups (row names) in \code{target} not
#' present in the survey data crosstables will be dropped from the analysis.
#'
#' @seealso \code{\link{mrp_model}}, \code{\link{wtd_tab}}
#' @param data A data frame containing the original survey data used in a call
#'   to \code{mrp_model} that has had the cell predictions from the regression
#'   fit merged with the data frame. See Details.
#' @param xvars character: Vector of variable names in \code{data} for x-axis
#'   rows to pass to \code{\link{wtd_tab}}.
#' @param yvar character: Response variable for the y-axis in a two-way table to
#'   pass to \code{wtd_tab}.
#' @param target A data frame or table containing the true values of the
#'   weighted proportions between \code{xvars} and \code{yvars}. If
#'   \code{target} is an object of class \code{data.frame}, it will be coerced
#'   into as table. See Details.
#' @return A table containing diagnostic statistics describing the accuracry of
#'   the estimates using raked weights and multilevel regression with
#'   postratification cell predictions.
#'
#' @export
#' @examples
#' \donttest{
#' ### Run the MRP regression
#' mrp.fit1 <- mrp_model(y ~ z.st.repvote + (1 | age4) + (1 | sex) + (1 | race4) + (1 | educ3),
#'                       data = data, mrp.data = cps.tbl)
#'
#' ### Extract cell predictions and merge with data
#' res <- mrp_merge(data, mrp.fit1)
#'
#' ### Create target as a data frame
#' load("Data/pollstr_data.Rda")
#' # Aggregate single estimates by state
#' pollstr_data_agg <- pollstr_data %>%
#'   group_by(state, choice) %>%
#'   summarise(value = mean(value))
#' pollstr1 <- reshape2::dcast(pollstr_data_agg, state ~ choice, value.var = 'value')
#' pollstr1$state <- abbr2state(pollstr1$state)
#' for (i in 2:ncol(pollstr1))
#'   pollstr1[, i] <- round(pollstr1[, i], 2)
#'
#' ### Run evaluation
#' mrp_eval(data = res, xvars = 'demState', yvar = 'nr2Bin', target = pollstr1)
#'
#'
#' ### Create target as a table
#' load("Data/cps_nov12_clean.Rda")
#' cps <- cps_nov12_clean
#' cps <- copy.lev(cps, cps_nov12_clean)
#' cps$demState <- cps$state
#' levels(cps$demState) <- levels(res$demState)[1:51]
#' t0 <- wtd_tab(cps, 'demState', 'kids2', 'wts')
#' t0 <- t0 / rowSums(t0) * 100
#'
#' ### Run evaluation
#' mrp_eval(data = res, xvars = 'demState', yvar = 'demKids', target = t0)
#' }
mrp_eval <- function (data, xvars, yvar, target, cor.method = c("pearson", "kendall", "spearman"))
{
  if (any(!(c(xvars, yvar, 'wts', 'w_cellpred')) %in% names(data)))
    stop("One of xvars, yvar, wts, or w_cellpred is missing from data.")

  # Create tables for estimates
  t1 <- wtd_tab(data, xvars, yvar, 'w_cellpred')
  t1 <- t1 / rowSums(t1) * 100
  t2 <- wtd_tab(data, xvars, yvar, 'wts')
  t2 <- t2 / rowSums(t2) * 100

  # Format target table if not already a table
  t3 <- target
  if (!is.table(target)) {
    t3 <- as.table(as.matrix(t3[, 2:ncol(t3)]))
    rownames(t3) <- target[, 1]
  }
  pos1 <- rownames(t1) %in% rownames(t3)
  if (length(pos1) != 0) {
    if (length(which(!pos1)) == nrow(t1))
      stop("No matching rownames in tables for specified survey variables and target table.")
    t1 <- t1[pos1, ]
    t2 <- t2[pos1, ]
  }
  ## Match order of rownames
  t1 <- t1[order(rownames(t3)), ]
  t2 <- t2[order(rownames(t3)), ]

  # Calculate absolute errors, other statistics
  t1err <- t1 - t3
  t2err <- t2 - t3
  t1abs <- abs(t1 - t3)
  t2abs <- abs(t2 - t3)
  t1cor <- cor.test(t1, t3, method = cor.method)
  t2cor <- cor.test(t2, t3, method = cor.method)
  t1win <- sum(t1abs < t2abs)
  t2win <- sum(t1abs > t2abs)
  t1winp <- t1win / length(t1)
  t2winp <- t2win / length(t2)
  t1best <- ifelse(t1win > t2win, 1, 0)
  t2best <- ifelse(t1win > t2win, 0, 1)



  x1 <- c(mean(t1abs), sd(t1err), mad(t1err), sum(t1abs), t1cor$estimate,
          t1cor$conf.int[1:2], t1win, t1winp, t1best, nrow(data))
  x2 <- c(mean(t2abs), sd(t2err), mad(t2err), sum(t2abs), t2cor$estimate,
          t2cor$conf.int[1:2], t2win, t2winp, t2best, nrow(data))
  res <- rbind(x1, x2)
  dimnames(res) <- list(c('mrp', 'rake'),
                        c('mean_abs_error', 'sd_error', 'mad_error', 'sum_abs_error', 'correlation',
                          'conf_low', 'conf_high', 'n_best_est', 'pr_best_est', 'selected', 'sample_size'))
  as.table(res)
}