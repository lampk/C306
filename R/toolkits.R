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
simple_recode <- function(x, map, as = c('as_is', 'numeric', 'factor', 'character', 'logical'), ignore.case = FALSE, perl = TRUE, ...){
  requireNamespace('tidyr')
  as <- match.arg(as)
  if (missing(map)) stop ('A conversion map should be provided!')

  if (is.data.frame(map)) Map <- map
  else {
    Map <- data.frame(
      to = unlist(lapply(names(map),
                  function(name) rep(name, length(map[[name]])))),
      from = unlist(map),
      stringsAsFactors = FALSE
    )}

  if (length(Map$from) != length(unique(Map$from))) stop('Patterns should be unique.')

  x.recoded <- x
  for (i in 1:nrow(Map)){
    if (is.na(Map$from[i])) x.recoded <- tidyr::replace_na(x.recoded, Map$to[i])
    else x.recoded <- gsub(Map$from[i], Map$to[i], x.recoded, ignore.case = ignore.case, perl=perl)
  }


  if (as != 'as_is') {
    x.recoded <-
      switch(as,
             numeric = as.numeric(x.recoded),
             character = as.character(x.recoded),
             factor = factor(x.recoded, ...),
             logical = as.logical(x.recoded))
  } else {
    if (is.factor(x)) {
      tryCatch(if(length(levels(x))==2) x.recoded <- as.logical(x.recoded),
               error = function(e){
                 levels.x <- levels(x)
                 levels.recoded <- simple_recode(levels.x, map=Map)
                 x.recoded <- factor(x.recoded, levels = levels.recoded)
               })
    }
    if (is.numeric(x)) try(x.recoded <- as.numeric(x.recoded))
  }
  return(x.recoded)
}

#' Recode values with regex patterns and pipe support
#' @aliases recode_var
#' @description This support a robust replacement for data by providing relevant map in the form of var = map
#' @param .data A data frame to modify
#' @param ...
#' Replacement in the form of var = map. Maps will be passed to simple_recode().
#'
#' See simple_recode for more details
#'
#' @param ignore.case Specify whether the pattern is case-insensitive. Default is FALSE (case sensitive)
#' @param perl Are patterns follow the Perl-style regular expression.
#' @return A data frame with recoded variables.
#' @seealso \link{simple_recode}
#' @export
var_recode <- recode_var <- function(.data, ..., ignore.case = FALSE, perl = TRUE){
  .maps <- list(...)
  .vars <- names(.maps)
  for (.var in .vars){
    .data[[.var]] <- simple_recode(x = .data[[.var]], map = .maps[[.var]], ignore.case = ignore.case, perl = perl)
  }
  return(.data)
}


`do_to<-` <- function(x, value){
  match.fun(value)(x)
}
