#' Relevel in correspondence to another factor
#'
#' @description Function to relevel a variable in accordance to a another variable of class "factor".
#' @return An object of the same class as the input
#' @export
simple_relevel <- function(x, ...){
  UseMethod('simple_relevel')
}

#' @rdname simple_relevel
#'
#' @param data A data.frame
#' @param by A vector or unquoted variable name the relevelling process will base on
#' @param ... Unquoted variable names that will be relevelled
#' @export
simple_relevel.data.frame <- function(data, ..., by){
  #... variables to relevel
  # by: variable that is based on
  require(dplyr)
  requireNamespace('rlang')
  dep.vars <- rlang::enquos(...)
  dep.vars.2 <- sapply(dep.vars, function(dep.var) rlang::quo_name(dep.var))
  by <- rlang::enquo(by)
  by.2 <- rlang::quo_name(by)
  # browser()
  if (!is.factor(data[[by.2]])) data[[by.2]] <- as.factor(data[[by.2]])
  for(i in seq_along(dep.vars.2)){
    # browser()
    dep.var.2 <- dep.vars.2[[i]]
    dep.var <- dep.vars[[i]]
    isolated.data <-
      data[c(by.2, dep.var.2)] %>%
      unique() %>% #remove all dup
      arrange({{dep.var}}) %>% arrange({{by}})# first, sort the data by dep.var to retain prev levels, second by "by"
    dep.var.lv <- isolated.data[[dep.var.2]]
    # browser()
    data[[dep.var.2]] <- factor(data[[dep.var.2]], levels = dep.var.lv)
  }
  data
}

#' @rdname simple_relevel
#'
#' @param x a vector that will be relevelled
#' @param by a vector that the relevelling process will base on
#' @param map a list that links by to x in the form of list(by.1 = c(x1.1, x1.2...), by.2 = c(x2.1, x2.2...))
#' or a data.frame with two columns "from" and "to" serving the same purpose.
#' @export
simple_relevel.default <- function(x, by, map){
  requireNamespace('dplyr')
  if (is.list(map)){
    map <- do.call(rbind,
                   lapply(seq_along(map),
                          function(i){
                            .map <- map[i]
                            name <- names(.map)
                            data.frame(from = rep(name, length(map[[i]])), to = map[[i]])
                          }))
  }
  # browser()
  stopifnot(setequal(unique(x), map$to))
  stopifnot(setequal(unique(by), map$from))
  map$from <- factor(map$from, levels = levels(by))
  X <- data.frame(to = x)
  data <- merge(X, map, by = 'to', all.x = TRUE)
  simple_relevel.data.frame(data, to, by = from)$to
}
