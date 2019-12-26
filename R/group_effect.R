#' Calculation of covariable effects within each value of another covariable that has interaction
#' @description This function calculate the OR, CI, an p.value within each value of specified covariable.
#' @param formula
#' A formula in the the form of A + B ~ X, where
#'
#' - X is the covariable to be concerned. X must be of type factor.
#' - A, B are the covarables having interaction with X whose effects will be calculated.
#' If . or blank. All covariables having interaction with X would be included.
#'
#' @param model A fitted glm model
#' @param method
#' a string whose value is either
#' "lik.ratio" for CI and tests based on likelihood ratio statistics (preferred)
#' or "wald" for CI and tests based on Wald statistics
#' @param stat_digits  Number of decimal digits for statistics
#' @param p_digits Number of decimal digits for the p-values
#' @param simplify by default, if there is only one variable on each side of the formula and
#' no LHS is a binary, then the function will combine tables in each state of X to one.
#' Set to FALSE to avoid this behaviour.
#' @param sstable logical value specifying whether to return in sstable format. Default is FALSE. Set to TRUE forces verbose to FALSE
#' @param flextable logical value specifying whether to build flextable object. Default it FALSE. Set to TRUE forces sstable to TRUE.
#' @return Under certain circumstances defined in param simplify, a flextable, an sstable,
#' a data.frame of class 'subgroup_logist_summary',
#' or a 'subgroup_logist_summary'/list of logist_summary, each represents one state of X.
#' @author Trinh Dong Huu Khanh
#' @export
subgroup_effect <- function(formula, model, method = c('lik.ratio', 'wald'), stat_digits=2, p_digits=4, sstable=FALSE, flextable=FALSE, simplify = TRUE)
{
  # browser()
  method <- match.arg(method)
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
      logLik.full <-logLik(new_model)
      # browser()
      ci.lower <- lapply(summary_vars, function(sv){
        k <- which(names(new_mf) == sv) - 1
        iS <- which(attr(new_mm, 'assign') == k)
        ci.lower.sv <- suppressMessages(confint(new_model))[iS,1]
        names(ci.lower.sv) <- colnames(new_mm)[iS]
        ci.lower.sv
      })
      ci.upper <- lapply(summary_vars, function(sv){
        k <- which(names(new_mf) == sv) - 1
        iS <- which(attr(new_mm, 'assign') == k)
        ci.upper.sv <- suppressMessages(confint(new_model))[iS,2]
        names(ci.upper.sv) <- colnames(new_mm)[iS]
        ci.upper.sv
      })
      p <- lapply(summary_vars, function(sv){
        k <- which(names(new_mf) == sv) - 1
        iS <- which(attr(new_mm, 'assign') == k)
        p.sv <- sapply(iS, function(i){
          fit.i <- glm.fit(x = new_mm[, -i], y = y, family = binomial())
          logLik.i <- logLik(structure(fit.i, class = 'glm'))
          1 - pchisq(2 * (logLik.full - logLik.i), 1)
        })
        names(p.sv) <- colnames(new_mm)[iS]
        p.sv
      })
    } else {
      ci.lower <- lapply(summary_vars, function(sv){
        k <- which(names(new_mf) == sv) - 1
        iS <- which(attr(new_mm, 'assign') == k)
        ci.lower.sv <- suppressMessages(confint.default(new_model))[iS,1]
        names(ci.lower.sv) <- colnames(new_mm)[iS]
        ci.lower.sv
      })
      ci.upper <- lapply(summary_vars, function(sv){
        k <- which(names(new_mf) == sv) - 1
        iS <- which(attr(new_mm, 'assign') == k)
        ci.upper.sv <- suppressMessages(confint.default(new_model))[iS,2]
        names(ci.upper.sv) <- colnames(new_mm)[iS]
        ci.upper.sv
      })
      ps <- coef(summary(new_model))[, 'Pr(>|z|)']
      p <- lapply(summary_vars, function(sv){
        k <- which(names(new_mf) == sv) - 1
        iS <- which(attr(new_mm, 'assign') == k)
        p.sv <- ps[iS]
        p.sv
      })
    }

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
    message('- Effect table for ', crayon::green(rownames(out[[1]])))
    grp <- names(out)
    out <- do.call(rbind, out)
    rownames(out) <- grp
    lgs <- ._as_logist_summary(out, method = method, stringsAsFactor = FALSE)
    class(lgs) <- c('subgroup_logist_summary', class(lgs))
    attr(lgs, 'base_var') <- base_var
    attr(lgs, 'summary_vars') <- summary_vars
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

print.subgroup_logist_summary <- function(x, ...){
  if (is.data.frame(x)) NextMethod('print')
  else {
    method <- attr(x[[1]], 'method')
    base_var <- attr(x, 'base_var')
    cat(crayon::silver('- Method:', switch(method, lik.ratio = 'Likelihood ratio', wald = 'Wald')), '\n')
    cat(crayon::silver('- CIs are calculated at 95% level of confidence.\n\n'))
    for (i in seq_along(x)){
      name <- paste0(base_var, ' = ', names(x)[i])
      this <- cbind(rn = rownames(x[[i]]), x[[i]])
      colnames(this)[[1]] <- name
      print.data.frame(this, row.names = FALSE)
      cat('\n')
    }
  }
  invisible(x)
}
