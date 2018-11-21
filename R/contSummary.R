#' @export
contSummary <- function(x, statistics = c("med.IQR", "med.90", "med.range", "mean.sd"), digits = 1, n = TRUE){
  if (length(statistics) > 1) statistics <- "med.IQR"
  loc <- formatC(ifelse(statistics == "mean.sd",
                        mean(x, na.rm = TRUE),
                        median(x, na.rm = TRUE)), digits = digits, format = "f")
  dis <- switch(statistics,
                mean.sd   = formatC(sd(x, na.rm = TRUE), digits = digits, format = "f"),
                med.IQR   = paste(
                  formatC(quantile(x, probs = c(0.25, 0.75), na.rm = TRUE), digits = digits, format = "f"),
                  collapse = ", "),
                med.90    = paste(
                  formatC(quantile(x, probs = c(0.05, 0.95), na.rm = TRUE), digits = digits, format = "f"),
                  collapse = ", "),
                med.range = paste(
                  formatC(quantile(x, probs = c(0.00, 1.00), na.rm = TRUE), digits = digits, format = "f"),
                  collapse = ", "))
  if (n == TRUE){
    output <- paste(loc, " (", dis, ") - ", length(na.omit(x)), sep = "")
    names(output) <- paste(statistics, "- n")
  } else {
    output <- paste(loc, " (", dis, ")", sep = "")
    names(output) <- statistics
  }
  return(output)
}
