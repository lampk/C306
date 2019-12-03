#' Overlap join of 2 data frames
#' @description This function is a wrapper of data.table's foverlaps, following the dplyr's _join grammar.
#' @param x,y data.frames
#' @param by,by.x,by.y
#' a character vector of variables to join by, passed to \link[data.table]{foverlaps}' by.x ad by.y.
#'
#' If NULL, the default,  will do a natural join, using all variables with common names across the two tables.
#' A message lists the variables so that you can check they're right
#' (to suppress the message, simply explicitly list the variables that you want to join).
#'
#' Otherwise, the last two columns must be the start and end time of the event specified.
#'
#' @param type character-typed value, either 'left' (default), right, or 'inner',
#' representing left_join, right_join, and inner_join
#' @param overlap character-typed value, passed to \link[data.table]{foverlaps}' type
#'
#' Allowed values are 'any' (default), 'within', 'start', 'end' and 'equal'.
#'
#' The types shown here are identical in functionality
#' to the function findOverlaps in the bioconductor package IRanges.
#'
#' Let [a,b] and [c,d] be intervals in x and y with a<=b and c<=d.
#' For type="start", the intervals overlap iff a == c.
#' For type="end", the intervals overlap iff b == d.
#' For type="within", the intervals overlap iff a>=c and b<=d.
#' For type="equal", the intervals overlap iff a==c and b==d.
#' For type="any", as long as c<=b and d>=a, they overlap.
#' In addition to these requirements, they also have to satisfy the min_overlap argument.
#'
#' @param multiple_match a character-typed value, passed to \link[data.table]{foverlaps}' mult
#'
#' When multiple rows in y match to the row in x,
#' multiple_match controls which values are returned - "all" (default), "first" or "last".
#'
#' @param max_gap a character-typed value, passed to \link[data.table]{foverlaps}' maxgap.
#' It should be a non-negative integer value, >= 0. Default is 0 (no gap). For intervals [a,b] and [c,d],
#' where a<=b and c<=d, when c > b or d < a, the two intervals don't overlap.
#' If the gap between these two intervals is <= maxgap, these two intervals are considered as overlapping.
#'
#' @param min_overlap a character-typed value, passed to \link[data.table]{foverlaps}' minoverlap
#'
#' It should be a positive integer value, > 0. Default is 1. For intervals [a,b] and [c,d],
#' where a<=b and c<=d, when c<=b and d>=a, the two intervals overlap.
#' If the length of overlap between these two intervals is >= minoverlap,
#' then these two intervals are considered to be overlapping.
#'
#' @return a tibble with relevant columns from x, y
#' @seealso \link[data.table]{foverlaps}
#' @export
overlap_join <- function(x, y, by = NULL, by.x = by, by.y = by,
                         type = c('left', 'right', 'inner'),
                         overlap = c('any', 'within', 'start', 'end', 'equal'),
                         multiple_match = c('all', 'first', 'last'),
                         max_gap = 0L, min_overlap = 1L)
{
  type <- match.arg(type)
  overlap <- match.arg(overlap)
  mutl <- match.arg(multiple_match)
  arg.x <- switch(type, left = x, y)
  arg.y <- switch(type, left = y, x)
  arg.by.x <- switch(type, left = by.x, by.y)
  arg.by.y <- switch(type, left = by.y, by.x)
  nomatch <- switch(type, inner = NULL, NA)
  if (!length(by)) {
    if (!length(by.x) & !length(by.y)){
      by.x <- by.y <- by <- intersect(names(x), names(y))
      message('Joining by =', by)
    }
    if (xor(length(by.x), length(by.y))) stop('Both by.x and by.y must not be missing.')
  }

  data.table::setDT(arg.x)
  data.table::setDT(arg.y)
  data.table::setkeyv(arg.x, arg.by.x)
  data.table::setkeyv(arg.y, arg.by.y)

  merged_dt <- data.table::foverlaps(x = arg.x, y = arg.y, by.x = arg.by.x, by.y = arg.by.y,
                                     maxgap = max_gap, minoverlap = min_overlap, nomatch = nomatch,
                                     which = FALSE, verbose = FALSE)
  merged_dt <- dplyr::select(merged_dt, !!!union(names(x), names(y)))
}

#' Summarise events overlapping of 2 event data frames.
#' @description A function to summarise the overlapping status of 2 event data frames
#' @param x,y data.frames.
#' @param ids the id col(s) wrapped in dplyr::vars() distinguishing different events.
#' Based on this is the summarisation taken.
#' @param cols,cols.x,cols.y variables wrapped in dplyr::vars(), passed to overlap_join.
#' By default, except for the last 2, every columns will be automatically included into ids.
#' @param sstable logical value specifying whether to return in sstable format. Default is FALSE.
#' @param flextable logical value specifying whether to build flextable object. Default it FALSE. Set to TRUE forces sstable to TRUE.
#' @return A summary table that has at least 6 columns.
#'
#' id column(s): identification of each event.
#'
#' X: total time when X happens, regardless Y
#' Y: total time when Y happens, regardless X
#' X_Y: time duration when X and Y happen simultaneously
#' X_notY: time duration when only X happens
#' notX_Y: time duration when only Y happens
#' @seealso \link[dplyr]{vars}
#' @export
overlap_summary <- function(x, y, ids, cols, cols.x = cols, cols.y = cols, sstable = FALSE, flextable = FALSE)
{
  # browser()
  if (missing(cols)){
    if (missing(cols.x) & missing(cols.y)){
      cols.x <- cols.y <- cols <- intersect(names(x), names(y))
      message('Matching cols =', by)
    }
    if (xor(length(cols.x), length(cols.y))) stop('Both cols.x and cols.y must not be missing.')
  }

  cols.x <- names(dplyr::select(x, !!!cols.x))
  cols.y <- names(dplyr::select(y, !!!cols.y))

  if (missing(ids)) ids <- union(cols.x[1:(length(cols.x)-2)], cols.y[1:(length(cols.y)-2)])
  else ids <- union(cols.x[1:(length(cols.x)-2)], c(cols.y[1:(length(cols.y)-2)], sapply(ids, rlang::quo_name)))

  x_join <- overlap_join(x, y, by.x = cols.x, by.y = cols.y, type = 'left', overlap = 'any', multiple_match = 'all')
  y_join <- overlap_join(x, y, by.x = cols.x, by.y = cols.y, type = 'right', overlap = 'any', multiple_match = 'all')

  time_start.x <- cols.x[length(cols.x)-1]
  time_end.x <- cols.x[length(cols.x)]
  time_start.y <- cols.y[length(cols.y)-1]
  time_end.y <- cols.y[length(cols.y)]


  x_summary <- x_join %>%
    dplyr::mutate(
      .x_y_start = pmax(x_join[[time_start.x]], x_join[[time_start.y]]),
      .x_y_end = pmin(x_join[[time_end.x]], x_join[[time_end.y]]),
      .x_y_dur = .x_y_end - .x_y_start,
      .x_dur = x_join[[time_end.x]] - x_join[[time_start.x]],
      .x_y_dur = ifelse(is.na(.x_y_dur), 0, .x_y_dur)
    ) %>%
    dplyr::group_by_at(vars(!!!{{c(ids, time_start.x, time_end.x)}})) %>%
    dplyr::summarise(X_Y = sum(.x_y_dur), X = unique(.x_dur)) %>%
    dplyr::mutate(X_notY = X - X_Y) %>%
    dplyr::ungroup() %>%
    dplyr::group_by_at(vars(!!!{{ids}})) %>%
    dplyr::summarise_at(vars(X, X_Y, X_notY), sum)

  y_summary <- y_join %>%
    dplyr::mutate(
      .x_y_start = pmax(y_join[[time_start.x]], y_join[[time_start.y]]),
      .x_y_end = pmin(y_join[[time_end.x]], y_join[[time_end.y]]),
      .x_y_dur = .x_y_end - .x_y_start,
      .y_dur = y_join[[time_end.y]] - y_join[[time_start.y]],
      .x_y_dur = ifelse(is.na(.x_y_dur), 0, .x_y_dur)
    ) %>%
    dplyr::group_by_at(vars(!!!{{c(ids, time_start.y, time_end.y)}})) %>%
    dplyr::summarise(X_Y = sum(.x_y_dur), Y = unique(.y_dur)) %>%
    dplyr::mutate(notX_Y = Y - X_Y) %>%
    dplyr::ungroup() %>%
    dplyr::group_by_at(vars(!!!{{ids}})) %>%
    dplyr::summarise_at(vars(Y, notX_Y), sum)

  summary_tbl <-
    merge(x_summary, y_summary) %>%
    dplyr::select(!!!{{ids}}, X, Y, X_Y, X_notY, notX_Y)
  class(summary_tbl) <- c('overlap_summary', class(summary_tbl))
  attr(summary_tbl, 'footer') <-
    c('X: total time when X happens, regardless Y',
      'Y: total time when Y happens, regardless X',
      'X_Y: time duration when X and Y happen simultaneously',
      'X_notY: time duration when only X happens',
      'notX_Y: time duration when only Y happens')
  if (sstable) return(as_sstable.overlap_summary(summary_tbl, flextable = flextable))
  summary_tbl
}


