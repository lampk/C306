#' Identify new lab adverse event to report
#'
#' @description A function to identify new lab adverse event to report from a vector of consecutive lab adverse event grade.
#'
#' @param data a numeric vector of consecutive lab adverse event grade.
#' @param gradecut a grade threshold to report.
#'
#' @return A factor specifies whether new lab adverse event to report is identified or not.
#' @export
labAE <- function(data, gradecut){
  cur <- data; pre <- lag(cur, k = 1)
  out <- ifelse(is.na(cur), NA,
                ifelse((is.na(pre) | (!is.na(pre) & (pre < cur))) & (cur >= gradecut), "Yes", "No"))
  return(factor(out, levels = c("Yes", "No")))
}
