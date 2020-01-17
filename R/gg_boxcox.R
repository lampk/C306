#' Compute and draw Box-Cox transformation plot for linear models using ggplot2
#' @description A forked version of MASS::boxcox using ggplot2
#' @param object a formula or fitted model object. Currently only lm and aov objects are handled.
#' @param lambda vector of values of lambda – default (-2, 2) in steps of 0.1
#' @param interp
#' logical which controls whether spline interpolation is used. Default to TRUE if plotting with lambda of length less than 100.
#' @param eps Tolerance for lambda = 0; defaults to 0.02
#' @param xlab defaults to "lambda"
#' @param ylab defaults to "log-Likelihood"
#' @param ... additional parameters to be used in the model fitting.
#' @return a ggplot object
#' @seealso \link[MASS]{boxcox}
#' @examples
#' gg_boxcox(Volume ~ log(Height) + log(Girth), data = trees,
#'           lambda = seq(-0.25, 0.25, length = 10))
#' @export
gg_boxcox <- function(object,...){
  UseMethod('gg_boxcox')
}

#' @export
gg_boxcox.formula <- function (object, lambda = seq(-2, 2, 1/10),
          interp = (m < 100), eps = 1/50, xlab = expression(lambda),
          ylab = "log-Likelihood", ...)
{
  m <- length(lambda)
  object <- lm(object, y = TRUE, qr = TRUE, ...)
  NextMethod()
}

#' @export
gg_boxcox.lm <- function(object, lambda = seq(-2, 2, 1/10),
                         interp = (m < 100), eps = 1/50, xlab = expression(lambda),
                         ylab = "log-Likelihood", ...)
{
  m <- length(lambda)
  if (is.null(object$y) || is.null(object$qr))
    object <- update(object, y = TRUE, qr = TRUE, ...)
 NextMethod()
}

#' @export
gg_boxcox.default <- function (object, lambda = seq(-2, 2, 1/10),
                       interp = (m < 100), eps = 1/50, xlab = expression(lambda),
                       ylab = "log-Likelihood", ...)
{
  object <- lm(object, y = TRUE, qr = TRUE, ...)
  # if (is.null(object$y) || is.null(object$qr))
  #   object <- update(object, y = TRUE, qr = TRUE, ...)
  y <- object$y
  xqr <- object$qr
  if (any(y <= 0))
    stop("response variable must be positive")
  n <- length(y)
  y <- y/exp(mean(log(y)))
  logy <- log(y)
  xl <- loglik <- as.vector(lambda)
  m <- length(xl)
  for (i in 1L:m) {
    if (abs(la <- xl[i]) > eps)
      yt <- (y^la - 1)/la
    else yt <- logy * (1 + (la * logy)/2 * (1 + (la * logy)/3 *
                                              (1 + (la * logy)/4)))
    loglik[i] <- -n/2 * log(sum(qr.resid(xqr, yt)^2))
  }
  if (interp) {
    sp <- spline(xl, loglik, n = 100)
    xl <- sp$x
    loglik <- sp$y
    m <- length(xl)
  }
  mx <- (1L:m)[loglik == max(loglik)][1L]
  Lmax <- loglik[mx]
  lim <- Lmax - qchisq(19/20, 1)/2
  plims <- par("usr")
  y0 <- plims[3L]
  plt <-
    ggplot2::ggplot()+
    ggplot2::geom_line(mapping = ggplot2::aes(x = xl, y = loglik), size=.5, color = grey(0.1))+
    ggplot2::labs(x = xlab, y = ylab)+
    ggplot2::geom_hline(ggplot2::aes(yintercept = lim), linetype = 'dashed')+
    ggplot2::geom_text(ggplot2::aes(x=min(lambda),y=lim, label='95%'), hjust='left', vjust='bottom')+
    ggplot2::ylim(min(loglik, lim), max(loglik, lim))
  la <- xl[mx]
  # browser()
  if (mx > 1 && mx < m)
    plt <- plt+
    ggplot2::geom_segment(ggplot2::aes(x=la, y=min(loglik), xend=la, yend=Lmax), linetype='dashed', color='red', size=.5)+
    ggplot2::geom_text(ggplot2::aes(x=la,y=min(loglik),label=round(la,digits = 2)),color='red',vjust='top',
              hjust='center')
  ind <- range((1L:m)[loglik > lim])
  if (loglik[1L] < lim) {
    i <- ind[1L]
    x <- xl[i - 1] + ((lim - loglik[i - 1]) * (xl[i] -
                                                 xl[i - 1]))/(loglik[i] - loglik[i - 1])
    plt <- plt+
      ggplot2::geom_segment(ggplot2::aes(x=x, y=min(loglik), xend=x, yend=lim), linetype='dashed', color=grey(.5), size=.5)
  }
  if (loglik[m] < lim) {
    i <- ind[2L] + 1
    x <- xl[i - 1] + ((lim - loglik[i - 1]) * (xl[i] -
                                                 xl[i - 1]))/(loglik[i] - loglik[i - 1])
    plt <- plt+
      ggplot2::geom_segment(ggplot2::aes(x=x, y=min(loglik), xend=x, yend=lim), linetype='dashed', color=grey(.5), size=.5)
  }
  plt
}

