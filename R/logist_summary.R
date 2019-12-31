#' An alternative summary for a logistic model with OR, CI, and p-values
#' @description a function to create summary table for glm logistic model
#' @param fit a logistic regression model of class "glm"
#' @param method
#' a string whose value is either
#' "lik.ratio" for CI and tests based on likelihood ratio statistics (preferred)
#' or "wald" for CI and tests based on Wald statistics
#' @param stat_digits  Number of decimal digits for statistics
#' @param p_digits Number of decimal digits for the p-values
#' @param verbose logical value specifying whether to print out result and notation. Default is FALSE
#' @param sstable logical value specifying whether to return in sstable format. Default is FALSE. Set to TRUE forces verbose to FALSE
#' @param flextable logical value specifying whether to build flextable object. Default it FALSE. Set to TRUE forces sstable to TRUE and verbose to FALSE.
#' Like other sstable objects, you can still create flextable or huxtable afterwards by using \link{ss_flextable} or \link{ss_huxtable}
#' @param ... additional parameters passed to \link{ss_flextable}
#' @author Marcel Wolbers, Lam Phung Khanh, and Trinh Dong Huu Khanh
#' @seealso \link{ss_format}, \link{ss_huxtable}, \link{ss_flextable}
#' @return A data frame of additional class "logist_summary" (if sstable == FALSE), a matrix of class c("summary_tbl", "ss_tbl") otherwise
#' @export
logist_summary <- function(fit, method = c('lik.ratio', 'wald'), stat_digits=2, p_digits=4, verbose = FALSE,
                           sstable = FALSE, flextable = FALSE, ...){
  compatible <- FALSE
  if (length(fit$family)) {
    if ((fit$family$family=='binomial')|(fit$family$link=='logit')) compatible <- TRUE
  }
  # browser()
  if (!compatible) stop("This function only works for logistic regression.")
  method <- match.arg(method)
  if (flextable) sstable <- TRUE
  if (sstable) verbose <- FALSE

  summary_method <- switch(method, lik.ratio = ._lik.ratio_summary, wald = ._wald_summary)
  result_obj <- summary_method(fit, stat_digits = stat_digits, p_digits = p_digits, verbose = verbose)

  out <- structure(result_obj$table, footer = result_obj$footer, method = method, class = c('logist_summary','data.frame'))
  if (sstable) return(as_sstable.logist_summary(out, flextable = flextable))
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
  return(list(table = result,
              footer = "Note: 95% confidence intervals and p-values are based on Wald statistics."))
}

._lik.ratio_summary <- function(fit, stat_digits=2, p_digits=4, verbose = FALSE, sstable = FALSE){
  est <- coef(fit)
  ci <- suppressMessages(confint(fit))
  result <- data.frame(log.OR = est, OR = exp(est),
                       lower.CI = exp(ci[,1]), upper.CI = exp(ci[,2]))

  has.intercept <- (names(coef(fit))[1]=="(Intercept)")
  any.ia <- any(attr(terms(formula(fit)),"order")>1)
  # browser()
  logLik.full <- logLik(structure(fit, class = 'glm'))
  x <- names(coef(fit))
  y <- model.response(model.frame(fit, drop.unused.levels = TRUE))
  p <- sapply(seq_along(x), function(i){
    fit.i <- glm.fit(x = model.matrix(fit)[,-i], y = y, family = binomial())
    logLik.i <-logLik(structure(fit.i, class = 'glm'))
    1 - pchisq(2 * (logLik.full - logLik.i), 1)
  })
  names(p) <- x
  result <- merge(result, data.frame(p.value = p), all.x = TRUE, by='row.names')
  rownames(result) <- result[,'Row.names']
  result <- result[, -1]

  for (i in 1:4) result[,i] <- formatC(result[,i], digits = stat_digits, format = 'f')
  if (length(result$p.value)) result$p.value <- formatC(result$p.value, p_digits, format = 'f')
  result$p.value <- ifelse(grepl('NA', result$p.value), '', result$p.value)
  if (naprint(fit$na.action)!="") message("\nNote:",naprint(fit$na.action))
  if (verbose) {
    cat("\nNote: 95% confidence intervals and p-values are based on likelihood ratio statistics.\n\n")
    if (!has.intercept) cat("\nNote: p-values of LR-tests not calculated because model has no intercept.\n\n")
  }

  return(list(table = result,
              footer = c(
                "Note: 95% confidence intervals and p-values are based on likelihood ratio statistics.",
                if (!has.intercept)
                  "p-values of LR-tests not calculated because model has no intercept.")))
  # return(result)
}

# provide a method for family to get the family of glm.fit
family.list <- function(object,...) object$family


#' Print method for logist_summary
#' @description Print method for logist_summary table
#' @param x an object of class logist_summary
#' @param ... additional params passed to print.data.frame
#' @return invisibly return itself
#' @seealso \link{print.data.frame}
#' @export
print.logist_summary <- function(x, ...)
{
  method <- attr(x, 'method')
  cat(crayon::silver('- Method:', switch(method, lik.ratio = 'Likelihood ratio', wald = 'Wald')), '\n')
  cat(crayon::silver('- CIs are calculated at 95% level of confidence.\n\n'))
  print.data.frame(x, ...)
}
