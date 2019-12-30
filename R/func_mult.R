#' Create or transform variables, with support for multiple assignment
#'
#' @description A function to create new variable(s) based on existing ones.
#' @param .data A data.frame, data.table, or tbl
#' @param ... Name-value pairs of expressions, following data.table syntax: "new_col" := expression.
#' Multiple assignments can be done via c("new_col1", "new_col2") := expression_returning_a_list.
#'
#' If unnamed, names of each list elements will be used
#' @return a data.frame, data.table, or a tbl
#' @examples
#' df <- data.frame(baz = 1:10, foo = c(rep(1, 5), rep(2, 5)))
#' df %>%
#'    dplyr::group_by(foo) %>%
#'    mutate_f(c('foo', 'bar') := list(a = cumsum(baz), b = baz*2))
#' @importFrom data.table :=
#' @export
mutate_f <- function(.data, ...){
  UseMethod('mutate_f')
}

#' @rdname mutate_f
#' @export
mutate_f.data.table <- function(.data, ...){
  dt_out <- mutate_f.data.frame(.data, ...)
  setDT(dt_out)
  dt_out
}

#' @rdname mutate_f
#' @export
mutate_f.tbl <- function(.data, ...){
  dt_out <- mutate_f.data.frame(.data, ...)
  dt_out <- dplyr::as_tibble(dt_out)
  dt_out
}

#' @rdname mutate_f
#' @export
mutate_f.grouped_df <- function(.data, ...){
  dt_out <- mutate_f.data.frame(.data, ...)
  group_names <- dplyr::group_vars(.data)
  dplyr::group_by_at(dt_out, dplyr::vars(!!!{{group_names}}))
}

#' @rdname mutate_f
#' @export
mutate_f.data.frame <- function(.data, ...){
  group_names <- dplyr::group_vars(.data)
  if (!data.table::is.data.table(.data)) data_tbl <- data.table::as.data.table(.data) else data_tbl <- .data
  data_tbl <- data_tbl[, ..., by = group_names]
  new_cols <- names(data_tbl)[!names(data_tbl) %in% union(group_names, names(.data))]
  data_tbl <- cbind(as.data.frame(.data), as.data.frame(data_tbl)[,new_cols])
  data_tbl
}

#########################################################

#' Reduce multiple values by summarisation functions.
#'
#' @description Create variables summarising exisiting variables, by using summarisation function.
#' @param .data A data.frame, data.table, or tbl
#' @param ... Summarisation functions.
#' Each of which must return a named list, whose names stand for new variables' names.
#' Anonymous functions must be wrapped in parentheses.
#'
#' @return a data.frame, data.table, or a tbl
#' @examples
#' df <- data.frame(baz = 1:10, foo = c(rep(1, 5), rep(2, 5)), bar =4:13)
#' df %>%
#'    dplyr::group_by(foo) %>%
#'    summarise_f((function(baz) list(a = cumsum(baz), b = baz*2))(baz), rangemisc::overlap_collapse(bar, baz))
#' @importFrom data.table :=
#' @export
summarise_f <- summarize_f <- function(.data, ...){
  UseMethod('summarise_f')
}

#' @rdname summarise_f
#' @export
summarise_f.data.frame <- function(.data, ...){
  # browser()
  .fun <- lapply(rlang::enquos(...), rlang::quo_get_expr)
  group_names <- dplyr::group_vars(.data)
  if (!data.table::is.data.table(.data)) data_tbl <- data.table::as.data.table(.data) else data_tbl <- .data
  expr <- rlang::quo_get_expr(rlang::quo(data_tbl[, c(!!!.fun), by = group_names]))
  rlang::eval_tidy(expr)
}

#' @rdname summarise_f
#' @export
summarise_f.data.table <- function(.data, ...){
  dt_out <- summarise_f.data.frame(.data, ...)
  setDT(dt_out)
  dt_out
}

#' @rdname summarise_f
#' @export
summarise_f.tbl <- function(.data, ...){
  dt_out <- summarise_f.data.frame(.data, ...)
  dt_out <- dplyr::as_tibble(dt_out)
  dt_out
}

#' @rdname summarise_f
#' @export
summarise_f.grouped_df <- function(.data, ...){
  dt_out <- summarise_f.data.frame(.data, ...)
  group_names <- dplyr::group_vars(.data)
  dplyr::group_by_at(dt_out, dplyr::vars(!!!{{group_names}}))
}
