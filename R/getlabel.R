#' Functions to get variable label
#'
#' @param x Variable or data to get label
#' @param meta Metadata of the dataset
#' @param units Append unit in the label?
#' @param fit Use presentation as in model fitting?
#'
#' @return vector of character of label

getlabel.default <- function(x, units = TRUE, fit = FALSE){
  if (is.null(attr(x, "type"))) attr(x, "type") <- ""
  if (is.null(attr(x, "label"))) attr(x, "label") <- ""

  # to create polished label
  if ((is.factor(x) | attr(x, 'type') == 'factor') | units == FALSE) {
    # if factor or do not need unit
    as.character(attr(x, 'label'))
  } else {
    # if not factor and need unit
    new_unit <- ifelse(fit == FALSE, ifelse(is.null(attr(x, 'units')), '', attr(x, 'units')),
                       paste('+',
                             ifelse(is.null(attr(x, 'scale')), 1, attr(x, 'scale')),
                             ' ',
                             ifelse(is.null(attr(x, 'units')), '', attr(x, 'units')), sep = ''))
    ifelse(new_unit == "",
           as.character(attr(x, 'label')),
           as.character(paste(attr(x, 'label'), ' [', new_unit, ']', sep = '')))

  }
}

getlabel.one <- function(x, meta = NULL, units = TRUE, fit = FALSE){
  # to create polished label for variable x from meta information

  if (!is.null(meta)) {
    # extract meta info
    .label <- ifelse(is.na(meta$label[meta$name == x]), '', meta$label[meta$name == x])
    .units <- ifelse(is.na(meta$units[meta$name == x]), '', meta$units[meta$name == x])
    .scale <- ifelse(is.na(meta$scale[meta$name == x]), 1, meta$scale[meta$name == x])
    .type <- meta$type[meta$name == x]

    # create label
    if (.type == 'factor' | units == FALSE) {
      # if factor or do not need unit
      out <- as.character(.label)
    } else {
      # if not factor and need unit
      new_unit <- ifelse(fit == FALSE, .units, paste('+', .scale, ' ', .units, sep = ''))
      out <- as.character(paste(.label, ' [', new_unit, ']', sep = ''))
    }
  } else {
    # if no meta information
    out <- getlabel.default(x, units = units, fit = fit)
  }

  # return
  out
}

getlabel.data <- function(data, meta = NULL, units = TRUE, fit = FALSE){
  # to create polished label for variables in data frame data from meta information

  out <- names(data)

  if (!is.null(meta)) {
    namedat <- merge(data.frame(name = out), meta, by = 'name', all.x = TRUE)

    # extract meta info
    namedat <- transform(.label = ifelse(is.na(label), name, label),
                         .units = ifelse(is.na(units), '', units),
                         .scale = ifelse(is.na(scale),  1, scale))

    # create label
    if (units == FALSE) {
      out <- as.character(namedat$.label)
    } else {
      if (fit) {namedat$.units <- with(namedat, paste('+', .scale, ' ', .units, sep = ''))}
      out <- with(namedat, ifelse(type == 'factor', as.character(.label), as.character(paste(.label, ' [', .units, ']', sep = ''))))
    }
  } else {
    if (ncol(data) == 1) {
      out <- getlabel.default(data)
    } else {
      out <- sapply(data, getlabel.default)
    }
    if (any(out == "")){
      tmp <- colnames(data)
      out <- ifelse(out == "", tmp, out)
    }
  }

  # return
  out
}

#' @export
getlabel <- function(x, meta = NULL, units = TRUE, fit = FALSE){
  if (is.null(dim(x))) {
    getlabel.one(x, meta = meta, units = units, fit = fit)
  } else {
    getlabel.data(x, meta = meta, units = units, fit = fit)
  }
}
