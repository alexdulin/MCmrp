#' Multilevel Regression with Poststratification
#'
#' Implements a multilevel regression model with postratification to create
#' estimates of public opinion at differing strata using census and survey data.
#'
#' Model Specifications: \itemize{ \item Dependent variable: The dependent
#' variable needs to be a binary choice that comes from the survey data. The
#' ideal variable would be one that is known to have high variance from state to
#' state (or whatever geographic sub-level is desired), such as gay marriage
#' approval.
#'
#' \item Fixed effects (regional predictors): These are the variables that occur
#' first in the model, and comprise known values of external state-level data.
#' The variables included as the fixed effects need to have sufficient variance
#' across states, need to be influential (either statistically or theoretically)
#' to the dependent variable, and need to be available for all states and DC.
#'
#' (Note: these factors are what allow MRP to create estimates for
#' sub-geographic regions. To create cell predictions for congressional
#' districts, for example, one would use district-level data rather than
#' state-level. Because these factors are the principal contributors to the
#' sub-region estimates, choosing the correct regional indicators is extremely
#' important to forming a valid MRP model.)
#'
#' \item Random effects (individual-level variables): The random effect
#' variables comprise individual level demographic information about respondents
#' in the survey. These are characteristics such as age, race, gender, etc. Any
#' variables that are to be used in poststratification need to be included in
#' the model as random effects.
#'
#' Additional random effects can be included that are not used in
#' poststratification. These can be factors such as interactions between
#' independent individual level variables, state-level variables, etc.
#'
#' \item Family: Generally, a binomial-logit model will suffice, but in
#' applications where various marginal distributions in the data have few
#' observations (for example, black men aged 18-25 with a Post graduate degree
#' and income less that 40k from the southwest) a quasi-Poisson distribution may
#' be appropriate (see
#' \href{http://www.stat.columbia.edu/~gelman/research/published/misterp.pdf}{Gelman
#' and Ghitza 2013, footnote 3}).}
#'
#' @seealso \code{\link{mrp_table}}, \code{\link{mrp_merge}},
#'   \code{\link[lme4]{glmer}}
#' @param formula A formula specifying the regression model. The formula must be
#'   in a valid format for \code{\link[lme4]{glmer}}.
#' @param data A data frame containing the survey data. All variables in
#'   \code{formula} must be columns in the dataset.
#' @param mrp.data A data frame created by \code{\link{mrp_table}} of class
#'   \code{mrp.table} with marginal distributions of individual level
#'   characteristics. Any variables (column names) in \code{mrp.data} that are
#'   also in \code{formula} (either as random or fixed effects) will be used in
#'   the poststratification stage to create weighted cell predictions.
#' @param method character: The regression function to use. Options are
#'   \code{'glmer'} for the \code{glmer()} function or \code{'bglmer'} for the
#'   \code{\link[blme]{bglmer}} function from the \code{blme} package.
#' @param family a GLM family, see \code{\link[stats]{glm}} and
#'   \code{\link[stats]{family}}.
#' @param ... Other options passed to \code{glmer} or \code{bglmer}.
#' @return A list object of class \code{mrp.model} containing the fitted model
#'   from \code{glmer}; a three-column data frame with the grouping variable
#'   index and the weighted and unweighted cell predictions; and the function
#'   call.
#'
#' @examples
#' \donttest{
#' mrp_load_libs()
#'
#' ### Needed to get 2012 vote share
#' library(RCurl)
#' library(XML)
#' library(stringr)
#'
#' ### Define model for poststratification
#' model <- list(~state, ~age4, ~sex, ~race4, ~educ3)
#' ### Load in merged trend dataset, recode to match CPS variables
#' data <- tsdat[tsdat$wts > 0 & !is.na(tsdat[, 'nr1']), ]
#' ## Take a random sample of the data
#' data <- sample_frac(data, size = .2)
#' data <- copy_lev(data, tsdat)
#' data.format()
#'
#' ## Recodes
#' data$state <- recode(data$demState, paste0(paste(1:51, 1:51, sep = "=", collapse = ";"), ";else=NA"))
#' levels(data$state) <- c(state.abb[1:8], "DC", state.abb[9:50])
#' data$sex <- data$demGender
#' data$age4 <- data$age
#' data$race4 <- data$demRace4
#' data$educ3 <- data$xeduc3
#' data$grp <- mrp_grp(data, model) # Make variable for later merging
#'
#' ## Make mariginal distribution table using 2012 CPS data
#' cps.tbl <- mrp_table(model = model)
#'
#' ## 2012 vote share by state, will be used as fixed-effect
#' presdat <- readHTMLTable(getURL("http://www.archives.gov/federal-register/electoral-college/2012/popular-vote.html"),
#'                          stringsAsFactors = FALSE)[[2]][1:51, 1:3]
#' names(presdat) <- c('state', 'democrat', 'republican')
#' for (i in 2:3)
#'   presdat[, i] <- as.numeric(presdat[, i])
#' presdat[, 2:3] <- presdat[, 2:3] / rowSums(presdat[, 2:3])
#' presdat$state <- gsub("NY**", "NY", presdat$state, fixed = TRUE)
#'
#' ## Add vote share to main data and population table
#' data$st.repvote <- presdat$republican[data$state]
#' data$z.st.repvote <- rescale(data$st.repvote)  # needs to be rescaled to play nice with glmer
#'
#' presdat$stname <- presdat$state
#' presdat <- presdat[, c('stname', 'republican')]
#' colnames(presdat)[2] <- 'st.repvote'
#' presdat[, 'z.st.repvote'] <- rescale(presdat[, 2])
#'
#' cps.tbl <- merge(cps.tbl, presdat, by = 'stname', all.x = TRUE, sort = FALSE) # see merge.mrp.table
#'
#' ## Run the MRP regression
#' mrp.fit1 <- mrp_model(y ~ z.st.repvote + (1 | age4) + (1 | sex) + (1 | race4) + (1 | educ3),
#'                       data = data, mrp.data = cps.tbl)
#'
#' ### Extract cell predictions and merge with data
#' res <- mrp_merge(data, mrp.fit1)
#' }
#' @export
mrp_model <- function(formula, data, mrp.data, method = c("glmer", "bglmer"), family = binomial(link = "logit"), ...)
{
  cl <- match.call()
  if (!is(mrp.data, 'mrp.table'))
    stop("mrp.data must be an object of class mrp.table.")
  method <- match.arg(method)
  if (method == "glmer")
    fit <- lme4::glmer(formula, data = data, family = family, ...)
  if (method == "bglmer")
    fit <- blme::bglmer(formula, data = data, family = family, ...)

  vals <- fixef(fit)["(Intercept)"]
  # Extract fixed effects
  fxf <- names(fixef(fit))
  if (length(fxf) > 1) {
    fxf <- fxf[fxf %in% names(mrp.data)]
    for (i in 2:length(fxf)) {
      ind <- fxf[i]
      vals <- c(vals + fixef(fit)[ind] * mrp.data[, ind])
    }
  }
  # Extract random effects
  rxf <- names(ranef(fit))
  rxf <- rxf[rxf %in% names(mrp.data)]
  for (i in seq_along(rxf)) {
    ind <- rxf[i]
    vals <- c(vals + ranef(fit)[[ind]][mrp.data[, ind], 1])
  }
  # Create cell predictions
  cellpred <- arm::invlogit(vals)
  w_cellpred <- mrp.data$prop * cellpred
  pred <- as.data.frame(cbind(cellpred, w_cellpred))
  pred <- cbind(pred, grp = mrp.data$grp)

  res <- list(mrp.fit = fit, predictions = pred, call = cl)
  class(res) <- "mrp.model"
  res
}