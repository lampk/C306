#' Transform an object to its explicit form
#' @description A function to transform an object to its explicit form if possible
#' @param x An object
#' @seealso \link{explicit.lm}
#' @export
explicit <- function(x, ...){
  UseMethod('explicit')
}

#' Transform a model to its explicit form
#' @description
#' A function to transform an lm model formula to its explicit form.
#' This is useful when you want to use stats::drop1 to drop additional terms in interaction models.
#' @param fit An object of class "lm"
#' @examples
#' iris.model <- lm(Sepal.Width ~ Sepal.Length*Petal.Width, data=iris)
#' explicit(iris.model)
#' @return A model with explicit formula
#' @seealso \link[stats]{add1}
#' @export
explicit.lm <- function(fit){
  ._explicit_lm(fit, data = data, .meta = FALSE)
  # if (length(data)) new_fit$call$data <- rlang::sym(deparse(substitute(data)))
  # new_fit
}

._explicit_lm <- function(lm, .meta = FALSE, ...)
{
  formula <- formula(lm)
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("lm", "data", "weights", "na.action",
               "offset"), names(mf), 0L)
  mf <- mf[c(1, m)]
  mf$drop.unused.levels <- TRUE
  # browser()
  mf[[1L]] <- as.name("model.frame")
  names(mf)[2L] <- "formula"
  mf <- eval(mf, parent.frame())
  y <- model.response(mf)
  x <- model.matrix(lm)
  new_data <- cbind(mf, model.matrix(lm)[,!colnames(x) %in% c('(Intercept)', colnames(mf))])
  x_names <- paste0('`', gsub('(^`)|(`$)', '', colnames(x)[-1]), '`')
  names(x_names) <- colnames(x)[-1]
  # browser()
  new_fml <- formula(call('~',
                          parse(text = paste(x_names,
                                             collapse = '+'),
                                keep.source = FALSE)[[1L]]
                          ))
  new_lm <- update(lm, new_fml, data = new_data)
  new_lm$call$data <- lm$call$data
  if (.meta) attr(new_lm, 'name.map') <- structure(names(x_names), names = unname(x_names))
  return(new_lm)
}
