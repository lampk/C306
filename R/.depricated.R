


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
