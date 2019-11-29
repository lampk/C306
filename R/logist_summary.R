#' An alternative summary for a logistic model with OR, CI, and p-values
#' @description a function to create summary table for glm logistic model
#' @param fit a logistic regression model of class "glm"
#' @param method
#' a string whose value is either
#' "lik.ratio" for CI and tests based on likelihood ratio statistics (preferred)
#' or "wald" for CI and tests based on Wald statistics
#' @param stat_digits  Number of decimal digits for statistics
#' @param p_digits Number of decimal digits for the p-values
#' @author Marcel Wolbers, Lam Phung Khanh, and Trinh Dong Huu Khanh
#' @return A data.frame of additional class "logist_summary"
#' @export
logist_summary <- function(fit, method = c('lik.ratio', 'wald'), stat_digits=2, p_digits=4, verbose = FALSE){
  compatible <- FALSE
  if (length(fit$family)) {
    if ((fit$family$family=='binomial')|(fit$family$link=='logit')) compatible <- TRUE
  }
  # browser()
  if (!compatible) stop("This function only works for logistic regression.")
  method <- match.arg(method)

  summary_method <- switch(method, lik.ratio = ._lik.ratio_summary, wald = ._wald_summary)
  result_obj <- summary_method(fit, stat_digits = stat_digits, p_digits = p_digits, verbose = verbose)

  out <- structure(result_obj, class = c('logist_summary', 'data.frame'))
  if (verbose) return(invisible(print(out)))
  return(out)
}

._wald_summary <- function(fit, stat_digits=2, p_digits=4, verbose = FALSE){
  est <- coef(fit)
  ci <- suppressMessages(confint.default(fit))
  result <- data.frame(log.OR = est, OR = exp(est),
                       lower.CI = exp(ci[,1]), upper.CI = exp(ci[,2]), p.value = summary(fit)$coef[,"Pr(>|z|)"])
  for (i in 1:4) result[,i] <- formatC(result[,i], digits = stat_digits, format = 'f')
  if (length(result$p.value)) result$p.value <- formatC(result$p.value, p_digits, format = 'f')
  if (naprint(fit$na.action)!="") message("\nNote:",naprint(fit$na.action))
  if (verbose){
    cat("\nNote: 95% confidence intervals and p-values are based on Wald statistics.\n\n",
        "\n(Inference based on likelihood ratio statistics can be obtained with logist.summary(",deparse(substitute(obj)),") and is usually more accurate.\n)",sep="")
  }
  return(result)
}

._lik.ratio_summary <- function(fit, stat_digits=2, p_digits=4, verbose = FALSE){
  any.ia <- any(attr(terms(formula(fit)),"order")>1)
  if (any.ia){
    message('Interaction terms detected. Using the explicit form.')
    return(._lik.ratio_summary(explicit(fit), stat_digits = stat_digits, p_digits = p_digits, verbose = verbose))
  }

  est <- coef(fit)
  ci <- suppressMessages(confint(fit))
  result <- data.frame(log.OR = est, OR = exp(est),
                       lower.CI = exp(ci[,1]), upper.CI = exp(ci[,2]))

  has.intercept <- (names(coef(fit))[1]=="(Intercept)")
  lr.test <- data.frame(drop1(fit,test="Chisq"))[-1,c("Df","Pr..Chi.")]

  if (has.intercept) result$p.value <- c(NA,lr.test[,"Pr..Chi."])

  for (i in 1:4) result[,i] <- formatC(result[,i], digits = stat_digits, format = 'f')
  if (length(result$p.value)) result$p.value <- formatC(result$p.value, p_digits, format = 'f')
  result$p.value <- ifelse(grepl('NA', result$p.value), '', result$p.value)
  if (naprint(fit$na.action)!="") message("\nNote:",naprint(fit$na.action))
  if (verbose) {
    cat("\nNote: 95% confidence intervals and p-values are based on likelihood ratio statistics.\n\n")
    if (!has.intercept) cat("\nNote: p-values of LR-tests not calculated because model has no intercept.\n\n")
  }
  return(result)
}

#' Transform an object to its explicit form
#' @description A function to transform an object to its explicit form if possible
#' @param x An object
#' @seealso \link{explicit.lm}
#' @export
explicit <- function(x){
  UseMethod('explicit')
}

#' Transform a model to its explicit form
#' @description
#' A function to transform an lm model formula to its explicit form.
#' This is useful when you want to use \link[stats]{drop1} to drop additional terms in interaction models.
#' @param fit An object of class "lm"
#' @examples
#' iris.model <- lm(Sepal.Width ~ Sepal.Length*Petal.Width, data=iris)
#' explicit(iris.model)
#' @return A model with explicit formula
#' @export
explicit.lm <- function(fit){
  ia <- ._get_interaction_terms(fit)
  ia.vars <- ia$ia.vars
  ia.terms <- ia$ia.terms
  fit.term_labels <- attr(terms(formula(fit)),"term.labels")
  # fit_data <- model.frame(fit)
  fit_data <- as.data.frame(fit$data)

  nonia.vars <- unique(fit.term_labels[!fit.term_labels %in% unlist(c(ia.vars, ia.terms))])

  # browser()
  var.classes <- attr(terms(fit), 'dataClasses')
  ia.classes <- var.classes[unique(unlist(ia.vars))]
  dummy_vars <- ._create_dummy(fit_data, names(var.classes)[!var.classes %in% c('numeric', 'other')])
  dummy_cols <- purrr::flatten(dummy_vars)
  dummy_cols.names <- sapply(dummy_vars, names, simplify = FALSE)
  explicit_data <- if (length(dummy_cols)) cbind(fit_data, dummy_cols) else fit_data

  # browser()
  ia.vars_dummy <-
    sapply(ia.vars,
           function(ia.var){
             sapply(ia.var,
                    function(iv){
                      if (iv %in% names(dummy_cols.names))
                        dummy_cols.names[[iv]]
                      else
                        iv
                    }, simplify = FALSE)
           }, simplify = FALSE)

  nonia.vars_dummy <-
    sapply(nonia.vars,
           function(nonia.var){
             sapply(nonia.var,
                    function(niv){
                      if (niv %in% names(dummy_cols.names))
                        dummy_cols.names[[niv]]
                      else
                        niv
                    }, simplify = FALSE)
           }, simplify = FALSE)

  ia.vars_dummy.interactTerms <-
    sapply(ia.vars_dummy,
           function(ia.var){
             expand_matrix <- expand.grid(ia.var)
             return(paste0('I(', paste(expand_matrix[,1], expand_matrix[,2], sep = '*'), ')'))
           }, simplify = FALSE)

  ia.vars_dummy.allTerms <-
    sapply(names(ia.vars_dummy),
           function(i) c(ia.vars_dummy[[i]], interaction = ia.vars_dummy.interactTerms[[i]]),
           simplify = FALSE)

  vars_dummy.allTerms <- c(nonia.vars_dummy, ia.vars_dummy.allTerms)

  # browser()
  all_terms.fml <- as.formula(
    paste0('~',
           paste(unique(unlist(vars_dummy.allTerms)), collapse = '+'))
  )
  # browser()

  explicit_model <- update(fit, all_terms.fml, data = explicit_data)
  return(explicit_model)
}

._get_interaction_terms <- function(fit){
  fml <- formula(fit)
  ia.terms <- attr(terms(fml),"term.labels")[attr(terms(fml),"order")>1]
  ia.factors <- attr(terms(fml),"factors")[,ia.terms, drop = FALSE]
  ia.vars <- sapply(ia.terms,
                    function(ia.term) rownames(ia.factors)[ia.factors[,ia.term] == 1],
                    simplify = FALSE)
  return(list(ia.terms = ia.terms, ia.vars = ia.vars))
}

._create_dummy <- function(data, cols){
  sapply(
    cols,
    function(.col){
      col_data <- as.factor(data[[.col]])
      col_value <- levels(col_data)[-1]
      new_cols <-
        lapply(col_value,
               function(val){
                 as.numeric(col_data == val)
               })
      names(new_cols) <- paste0(.col, col_value)
      return(new_cols)
    }, simplify = FALSE
  )
}

