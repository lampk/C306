._lik.ratio_summary <- function(fit, stat_digits=2, p_digits=4, verbose = FALSE){
  est <- coef(fit)
  ci <- confint(fit)
  result <- data.frame(log.OR = est, OR = exp(est),
                       lower.CI = exp(ci[,1]), upper.CI = exp(ci[,2]))

  has.intercept <- (names(coef(fit))[1]=="(Intercept)")
  any.ia <- any(attr(terms(formula(fit)),"order")>1)
  lr.test <- data.frame(drop1(fit,test="Chisq"))[-1,c("Df","Pr..Chi.")]
  if (any.ia){
    lr.test <- data.frame(drop1(explicit(fit), test='Chisq'))[-1,c("Df","Pr..Chi.")]
    rownames(lr.test) <-
      gsub('\\s{1}\\*\\s{1}', ':',
           gsub('(^I\\()|(\\)$)', '', rownames(lr.test), perl = TRUE),
           perl = TRUE)
  }

  browser()
  if (has.intercept) {
    p.value <-  lr.test[,"Pr..Chi.", drop = FALSE]
    names(p.value) <- 'p.value'
    row_order <- factor(rownames(result), levels = rownames(result))
    result <- merge(result, p.value, all.x = TRUE, by='row.names')
    result$Row.names <- factor(result$Row.names, levels = row_order)
    result <- dplyr::arrange(result, Row.names)
    rownames(result) <- result$Row.names
    result <- result[, -1]
  }

  for (i in 1:4) result[,i] <- formatC(result[,i], digits = stat_digits, format = 'f')
  if (length(result$p.value)) result$p.value <- formatC(result$p.value, p_digits, format = 'f')
  result$p.value <- ifelse(grepl('NA', result$p.value), '', result$p.value)
  if (naprint(fit$na.action)!="") message("\nNote:",naprint(fit$na.action))
  if (verbose) return(print(result))
  return(result)
}
