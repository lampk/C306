#' Do a simple recode via pattern for data
#'
#' @description
#' A function to re-encode data by using a map. Patterns are accepted. Unmentioned data are left intact.
#'
#' Method for class data.frame supports a robust replacement for data by providing relevant map in the form of var = map.
#'
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
#' @param .data A data frame to modify
#' @param ...
#' For method for data.frame: Replacement in the form of var = map. Maps must follow the syntax stipulated in the map parameter.
#'
#' For default method: Additional parameters passed to factor()
#'
#' @param ignore.case Specify whether the pattern is case-insensitive. Default is FALSE (case sensitive)
#' @param perl Are patterns follow the Perl-style regular expression.
#' @return A data frame with recoded variables.
#' @seealso \link{regex} \link{case_when}
#'
#' @return
#' If input is a vector, return a vector of the same length.
#'
#' If input is a data frame, return a data frame with relevant variables recoded.
#'
#' @export
simple_recode <- function(...){
  UseMethod('simple_recode')
}

#' @rdname simple_recode
#' @aliases recode_var var_recode
#' @method simple_recode data.frame
#' @export
simple_recode.data.frame <- function(.data, ..., ignore.case = FALSE, fixed = FALSE, perl = TRUE){
  .maps <- list(...)
  .vars <- names(.maps)
  for (.var in .vars){
    .data[[.var]] <- simple_recode.default(x = .data[[.var]], map = .maps[[.var]], ignore.case = ignore.case, fixed = fixed, perl = perl)
  }
  return(.data)
}

#' @rdname simple_recode
#' @method simple_recode default
#' @export
simple_recode.default <- function(x, map, as = c('as_is', 'numeric', 'factor', 'character', 'logical'), ignore.case = FALSE, fixed = FALSE, perl = TRUE, ...){
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
    else x.recoded <- gsub(Map$from[i], Map$to[i], x.recoded, ignore.case = ignore.case, fixed = fixed, perl=perl)
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
    if (is.numeric(x)) x.recode <- tryCatch(as.numeric(x.recoded), warning = function(w) x.recoded)
  }
  return(x.recoded)
}


`do_to<-` <- function(x, value){
  match.fun(value)(x)
}

#' Get or set the reference level
#' @description A function to quickly get or set the reference level of a factor
#' @export
ref_lv <- function(x){
  UseMethod('ref_lv')
}

#' @rdname ref_lv
#' @param x A factor
#' @return A character object the represents the reference level
#' @export
ref_lv.factor <- function(x){
  levels(x)[[1]]
}

#' @rdname ref_lv
#' @export
`ref_lv<-` <- function(x, value){
  UseMethod('ref_lv<-')
}

#' @rdname ref_lv
#' @param value An existing level to be set as reference
#' @export
`ref_lv<-.factor` <- function(x, value){
  relevel(x, value)
}

#' Get the proportion for each level or range of a vector
#' @description A function to calculate the proportion of each value range
#' @param x An object
#' @param method
#' A method to calculate the percentage.
#' Default is 'auto' which will attempt to choose the most fit method.
#' 'bin' is only meaningful for vector that only has to values.
#' 'fct' is meaningful for descrete variable
#' 'cont' is used for continuous variables. This will break the variable into several ranges
#' depended on the breaks specified in ...
#' @param na.rm A logical value deciding whether NA should be removed
#' @param ... Additional parameters passed to \link{cut}. Only work if method = 'cont'
#' See \link{cut}
#' @return
#' If method == 'bin': the percentage of non-reference level
#'
#' If method == 'fct': the percentages of all levels
#'
#' If method == 'cont': the percentages of all ranges based on specified breaks
#' @export
pct <- function(x, method = c('auto', 'bin', 'cont', 'fct'), na.rm = TRUE, ...){
  method <- match.arg(method)
  if (method == 'auto') method <- ._pct_get_method(unlist(x))

  if (method == 'bin' & !is.logical(x) & !setequal(unique(x), c(0,1))){
    if (length(unique(x)) > 2) stop('Forcing binary method for factor variable is meaningless.')
    if (length(unique(x)) == 1) return(100)
    if (length(unique(x)) != length(levels(x))) x <- factor(x, levels = unique(x), exclude=NULL)
    if (all(!is.na(levels(x))))
      warning('Binary method apply to non logical variable. This will calculate the pct of x != ref_lv')
    x <- x == levels(x)[2]
  }

  res <- switch(method,
                bin = ._pct_bin(x, na.rm),
                cont = ._pct_cont(x, na.rm, ...),
                fct = ._pct_fct(x, na.rm))

  return(res)
}

._pct_get_method <- function(x){
  if (is.logical(x) | length(unique(x)) <= 2) return('bin')
  if (is.factor(x) | is.character(x) | (is.numeric(x) & length(unique(x)) <= 5)) return('fct')
  return('cont')
}

._pct_bin <- function(x, na.rm = TRUE){
  n <- if (na.rm) sum(!is.na(x)) else length(x)
  return(sum(x, na.rm = TRUE)/n*100)
}

._pct_fct <- function(x, na.rm = TRUE){
  n <- if (na.rm) sum(!is.na(x)) else length(x)
  lvs <- if (na.rm) levels(x) else levels(addNA(x, ifany = TRUE))
  sapply(lvs,
         function(lv){
           sum(x == lv)/n*100
         })
}

._pct_cont <- function(x, na.rm = TRUE, ...){
  if (na.rm) x <- na.omit(x)
  x <- cut(x, ...)
  lv <- levels(addNA(as.factor(x), ifany = TRUE))
  if (any(is.na(lv))) lv <- c(NA, lv[!is.na(lv)])
  x2 <- factor(x, levels = lv, exclude = NULL)
  # browser()
  pct(x2, method = 'auto', na.rm = FALSE)
}
