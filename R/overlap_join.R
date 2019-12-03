#' Overlap join of 2 data frames.
#' @description This function is a wrapper of data.table's foverlaps, following the dplyr's _join grammar
#' @param x,y data.frames
#' @param by,by.x,by.y
#' a character vector of variables to join by.
#' If NULL, the default,  will do a natural join, using all variables with common names across the two tables.
#' A message lists the variables so that you can check they're right
#' (to suppress the message, simply explicitly list the variables that you want to join)
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
  arg.x <- switch(type, y, right = x)
  arg.y <- switch(type, x, right = y)
  arg.by.x <- switch(type, by.y, right = by.x)
  arg.by.y <- switch(type, by.x, right = by.y)
  nomatch <- switch(type, inner = NULL, NA)
  if (!length(by)) {
    if (!length(by.x) & !length(by.y)){
      by.x <- by.y <- by <- intersect(names(x), names(y))
      message('Joining by =', by)
    }
  }

  data.table::setDT(arg.x)
  data.table::setDT(arg.y)
  data.table::setkeyv(arg.x, arg.by.x)
  data.table::setkeyv(arg.y, arg.by.y)

  merged_dt <- data.table::foverlaps(x = arg.x, y = arg.y, by.x = arg.by.x, by.y = arg.by.y,
                                     maxgap = max_gap, minoverlap = min_overlap, nomatch = nomatch,
                                     which = FALSE, verbose = FALSE)
  dplyr::as_tibble(merged_dt)
}
