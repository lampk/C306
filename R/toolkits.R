#' Do a simple recode via pattern for data
#'
#' @description A function to re-encode x by using a map. Patterns are accepted.
#' @param x a vector
#' @param map
#' A map.
#'
#' Map can be a list in the form of recoded_text = c(to_be_recoded_text)
#'
#' Or as a data frame having 2 columns named "from" and "to".
#'
#' @param as A character string defining the post-recoded data type of x. Default is keeping as-is.
#' @param ignore.case,perl Parameters passed to gsub().
#' @param ... Additional parameters passed to factor().
#'
#' @return A vector or length == length(x)
#' @export
simple_recode <- function(x, map, as = c('as_is', 'numeric', 'factor', 'character'), ignore.case = FALSE, perl = TRUE, ...){
  as <- match.arg(as)
  if (missing(map)) stop ('A conversion map should be provided!')

  if (is.data.frame(map)) Map <- map
  else {
    Map <- data.frame(
      to = sapply(names(map),
                  function(name) rep(name, length(map[[name]])), USE.NAMES = FALSE),
      from = unlist(map)
    )}

  if (length(Map$from) != length(unique(Map$from))) stop('Patterns should be unique.')

  x.recoded <- x
  for (i in 1:nrow(Map))
    x.recoded <- gsub(Map$from[i], Map$to[i], x.recoded, ignore.case = ignore.case, perl=perl)

  if (as != 'as_is') {
    x.recoded <-
      switch(as,
             numeric = as.numeric(x.recoded),
             character = as.character(x.recoded),
             factor = factor(x.recoded, ...))
  } else {
    if (is.factor(x)) {
      levels.x <- levels(x)
      levels.recoded <- simple_recode(levels.x, map=Map)
      x.recoded <- factor(x.recoded, levels = levels.recoded)
    }
  }
  return(x.recoded)
}


`do_to<-` <- function(x, value){
  match.fun(value)(x)
}
