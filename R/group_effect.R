#' Calculation of covariable effects within each value of another covariable that has interaction
#' @description This function calculate the OR, CI, an p.value within each value of specified covariable.
#' @param formula
#' A formula in the the form of A + B ~ X, where
#'
#' - X is the covariable to be concerned. X must be of type factor.
#' - A, B are the covarables having interaction with X whose effects will be calculated.
#' If . or blank. All covariables having interaction with X would be included.
#'
#' @param model A fitted glm model or a formula
#' @param method
#' a string whose value is either
#' "lik.ratio" for CI and tests based on likelihood ratio statistics (preferred)
#' or "wald" for CI and tests based on Wald statistics
#' @param transpose logical value, default is FALSE, whether to return a transposed summary. See also \link{t.subgroup_logist_summary}
#' @param stat_digits  Number of decimal digits for statistics
#' @param p_digits Number of decimal digits for the p-values
#' @param simplify by default, if there is only one variable on each side of the formula and
#' no LHS is a binary, then the function will combine tables in each state of X to one.
#' Set to FALSE to avoid this behaviour.
#' @param sstable logical value specifying whether to return in sstable format. Default is FALSE. Set to TRUE forces verbose to FALSE
#' @param flextable logical value specifying whether to build flextable object. Default it FALSE. Set to TRUE forces sstable to TRUE.
#' @param ... additional parameters passed to glm to fit "model" (if model is a formula)
#' @return Under certain circumstances defined in param simplify, a flextable, an sstable,
#' a data.frame of class 'subgroup_logist_summary',
#' or a 'subgroup_logist_summary'/list of logist_summary, each represents one state of X.
#' @author Trinh Dong Huu Khanh
#' @seealso \link{logist_summary}, \link{print.subgroup_logist_summary}, \link{t.subgroup_logist_summary}
#' @examples
#' y = sample(0:1, 1000, replace = T)
#' x1 = sample(1:100, 1000, replace = T)
#' x2 = sample(c("A", "B"), 1000, replace = T)
#' x3 = sample(c("C", "D", "E"), 1000, replace = T)
#' x4 = sample(c("F", "G"), 1000, replace = T)
#' fakefit = glm(y ~ x1*x3 + x2*x3 + x2*x4, family = binomial())
#' C306::subgroup_effect(~x3, fakefit)
#' @export
subgroup_effect <- function(formula, model, method = c('lik.ratio', 'wald'), transpose = FALSE,
                            stat_digits = 2, p_digits = 4, sstable = FALSE, flextable = FALSE, simplify = TRUE, ...)
{
  # browser()
  method <- match.arg(method)
  if (inherits(model, 'formula')){
    fml <- model
    model <- glm(model, ..., family = binomial)
    model$call$formula <- fml
  }
  compatible <- FALSE
  if (length(model$family)) {
    if ((model$family$family=='binomial')|(model$family$link=='logit')) compatible <- TRUE
  }
  # browser()
  if (!compatible) stop("This function only works for logistic regression.")
  mf <- model.frame(model)
  all_ia <- ._get_interaction_terms(model)
  base_var <- formula.tools::rhs.vars(formula)
  base_lv <- levels(as.factor(mf[[base_var]]))
  summary_vars <- formula.tools::lhs.vars(formula)
  assertthat::assert_that(length(base_var) == 1,
                          msg = 'rhs must be of length 1')
  ._assert_ia(base_var, all_ia$ia.vars)
  has_base <- sapply(all_ia$ia.vars, function(ia_var) base_var %in% ia_var)
  base_ia <- sapply(all_ia, `[`, has_base, simplify = FALSE)
  summary_vars.auto <-
    unname(unlist(lapply(base_ia$ia.vars,function(ia.var) ia.var[ia.var != base_var])))
  if (length(summary_vars) &
      !(length(summary_vars) == 1 & !length(setdiff(summary_vars, '.'))))
  {
    ._assert_ia(summary_vars, summary_vars.auto)
  } else summary_vars <- summary_vars.auto
  y <- model.response(mf)
  # browser()
  ci.p <- sapply(base_lv, function(lv){
    new_mf <- mf
    new_mf[[base_var]] <- relevel(as.factor(new_mf[[base_var]]), ref = lv)
    new_model <- update(model, data = new_mf)
    new_mm <- model.matrix(new_model)
    est <- lapply(summary_vars, function(sv){
      k <- which(names(new_mf) == sv) - 1
      iS <- which(attr(new_mm, 'assign') == k)
      est.sv <- coef(new_model)[iS]
      names(est.sv) <- colnames(new_mm)[iS]
      est.sv
    })
    if (method == 'lik.ratio'){
      logLik.full <- logLik(new_model)
      ci.p <- sapply(summary_vars, function(sv){
        ci.p.sv <- ._ci_p_calc(sv, new_mf, new_mm, new_model, wald = FALSE, logLik.full = logLik.full)
      }, simplify = FALSE)
    } else {
      ci.p <- sapply(summary_vars, function(sv){
        ci.p.sv <- ._ci_p_calc(sv, new_mf, new_mm, new_model, wald = TRUE)
      }, simplify = FALSE)
    }
    ci.p.t <- purrr::transpose(ci.p)
    ci.lower <- ci.p.t$ci.lower.sv
    ci.upper <- ci.p.t$ci.upper.sv
    p <- ci.p.t$p.sv
    # browser()
    list(
      log.OR = do.call(c, as.list(unlist(est))),
      OR = do.call(c, as.list(exp(unlist(est)))),
      CI.lower = do.call(c, as.list(unlist(ci.lower))),
      CI.upper = do.call(c, as.list(unlist(ci.upper))),
      p.value = do.call(c, as.list(unlist(p))))
  }, simplify = FALSE)
  # browser()
  out <- lapply(ci.p, function(.x) do.call(cbind, .x))
  out <- sapply(out,
                function(.out){
                  for (i in 1:4)
                    .out[,i] <- formatC(as.numeric(.out[,i]),
                                        digits = stat_digits, format = 'f')
                  .out[,'p.value'] <- formatC(as.numeric(.out[,'p.value']),
                                              p_digits, format = 'f')
                  .out
                }, simplify = FALSE)
  # message('- CI and p-value are based on ',
  #         switch(method, wald = 'Wald', lik.ratio = 'Likelihood ratio'),
  #         ' test with 95% level of confidence.')
  if (all(sapply(out, nrow) == 1) & simplify){
    msg <- paste('- Effect table for', crayon::green(rownames(out[[1]])))
    grp <- names(out)
    out <- do.call(rbind, out)
    rownames(out) <- grp
    lgs <- ._as_logist_summary(out, method = method, stringsAsFactor = FALSE)
    class(lgs) <- c('subgroup_logist_summary', class(lgs))
    attr(lgs, 'base_var') <- base_var
    attr(lgs, 'summary_vars') <- summary_vars
    attr(lgs, 'msg') <- msg
    return(lgs)
  }
  tbl <- structure(sapply(out, ._as_logist_summary,
                   method = method, stringsAsFactors = FALSE,
                   simplify = FALSE),
            class = 'subgroup_logist_summary',
            base_var = base_var,
            summary_vars = summary_vars)
  if (flextable) sstable <- TRUE
  if (sstable) return(as_sstable.subgroup_logist_summary(tbl, flextable = flextable))
  if (transpose) return(t(tbl))
  tbl
}

._get_interaction_terms <- function(fit){
  fml <- formula(fit)
  ia.terms <- attr(terms(fml),"term.labels")[attr(terms(fml),"order")>1]
  ia.factors <- attr(terms(fml),"factors")[,ia.terms, drop = FALSE]
  ia.vars <- sapply(ia.terms,
                    function(ia.term) rownames(ia.factors)[ia.factors[,ia.term] == 1],
                    simplify = FALSE)
  return(list(ia.terms = ia.terms, ia.vars = ia.vars))
}

._assert_ia <- function(vars, ia_vars){
  for (v in vars){
    assertthat::assert_that(
      v %in% unique(unlist(ia_vars)),
      msg = paste(deparse(substitute(v)), 'not exists in interaction terms.')
    )
  }
}

._as_logist_summary <- function(x, method, ...){
  x <- as.data.frame(x, ...)
  class(x) <- c('logist_summary', class(x))
  attr(x, 'method') <- method
  attr(x, 'footer') <- paste('Note: 95% confidence intervals and p-values are based on', method, 'statistics.')
  x
}

._ci_p_calc <- function(sv, mf, mm, model, wald, logLik.full = NULL){
  # browser()
  y <- model.response(mf)
  k <- which(names(mf) == sv) - 1
  iS <- which(attr(mm, 'assign') == k)
  if (wald){
    ci <-  suppressMessages(confint.default(model))[iS,,drop = FALSE]
    ps <- coef(summary(model))[, 'Pr(>|z|)']
    p.sv <- ps[iS]
  } else {
    ci <- suppressMessages(confint(model))[iS,,drop = FALSE]
    p.sv <- sapply(iS, function(i){
      fit.i <- glm.fit(x = mm[, -i], y = y, family = binomial())
      logLik.i <- logLik(structure(fit.i, class = 'glm'))
      1 - pchisq(2 * (logLik.full - logLik.i), 1)
    })
  }
  ci.lower.sv <- ci[,1]
  ci.upper.sv <- ci[,2]
  names(ci.lower.sv) <- colnames(mm)[iS]
  names(ci.upper.sv) <- colnames(mm)[iS]
  names(p.sv) <- colnames(mm)[iS]
  list(ci.lower.sv = ci.lower.sv, ci.upper.sv = ci.upper.sv, p.sv = p.sv)
}

#' Print method for subgroup_logist_summary
#' @description Print method for subgroup_logist_summary
#' @param x an object of class subgroup_logist_summary
#' @param ... additional params passed to print.data.frame
#' @return invisibly return itself
#' @seealso \link{print.data.frame}
#' @export
print.subgroup_logist_summary <- function(x, ...){
  if (length(attr(x, 'msg'))) message(attr(x, 'msg'))
  if (is.data.frame(x)) NextMethod('print')
  else {
    method <- attr(x[[1]], 'method')
    base_var <- attr(x, 'base_var')
    cat(crayon::silver('- Method:', switch(method, lik.ratio = 'Likelihood ratio', wald = 'Wald')), '\n')
    cat(crayon::silver('- CIs are calculated at 95% level of confidence.\n\n'))
    for (i in seq_along(x)){
      name <- if (length(base_var)) paste0(base_var, ' = ', names(x)[i]) else names(x)[i]
      this <- cbind(rn = rownames(x[[i]]), x[[i]])
      colnames(this)[[1]] <- name
      print.data.frame(this, row.names = FALSE)
      cat('\n')
    }
  }
  invisible(x)
}

#' Transpose a subgroup logist summary table
#' @description A method to transpose logist summary table, rather than stratify by subgroup, it stratifies by covariables.
#' @param x an object of class subgroup_logist_summary
#' @return
#' If x is a simplified subgroup_logist_summary (aka a data.frame), return itself. Otherwise, a transposed list of class subgroup_logist_summary
#' @seealso \link{t}
#' @author Trinh Dong Huu Khanh
#' @examples
#' y = sample(0:1, 1000, replace = T)
#' x1 = sample(1:100, 1000, replace = T)
#' x2 = sample(c("A", "B"), 1000, replace = T)
#' x3 = sample(c("C", "D", "E"), 1000, replace = T)
#' x4 = sample(c("F", "G"), 1000, replace = T)
#' fakefit = glm(y ~ x1*x3 + x2*x3 + x2*x4, family = binomial())
#' se = C306::subgroup_effect(~x3, fakefit)
#' t(se)
t.subgroup_logist_summary <- function(x){
  if (is.data.frame(x)) return(x)
  v <- rownames(x[[1]])
  structure(sapply(v, function(.v) do.call(rbind, lapply(x,`[`,.v,)), simplify = FALSE), class = 'subgroup_logist_summary')
}
