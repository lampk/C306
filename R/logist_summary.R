#' An alternative summary for a logistic model with OR, CI, and p-values
#' @description a function to create summary table for glm logistic model
#' @param fit a logistic regression model of class "glm"
#' @param method
#' a string whose value is either
#' "lik.ratio" for CI and tests based on likelihood ratio statistics (preferred)
#' or "wald" for CI and tests based on Wald statistics
#' @param stat_digits  Number of decimal digits for statistics
#' @param p_digits Number of decimal digits for the p-values
#' @param verbose logical value specifying whether to print out result and notation. Default is FALSE
#' @param sstable logical value specifying whether to return in sstable format. Default is FALSE. Set to TRUE forces verbose to FALSE
#' @param flextable logical value specifying whether to build flextable object. Default it FALSE. Set to TRUE forces sstable to TRUE and verbose to FALSE.
#' Like other sstable objects, you can still create flextable or huxtable afterwards by using \link{ss_flextable} or \link{ss_huxtable}
#' @param ... additional params passed to \link{ss_flextable}
#' @author Marcel Wolbers, Lam Phung Khanh, and Trinh Dong Huu Khanh
#' @seealso \link{ss_format}, \link{ss_huxtable}, \link{ss_flextable}
#' @return A data frame of additional class "logist_summary" (if sstable == FALSE), a matrix of class c("summary_tbl", "ss_tbl") otherwise
#' @export
logist_summary <- function(fit, method = c('lik.ratio', 'wald'), stat_digits=2, p_digits=4, verbose = FALSE,
                           sstable = FALSE, flextable = FALSE, ...){
  compatible <- FALSE
  if (length(fit$family)) {
    if ((fit$family$family=='binomial')|(fit$family$link=='logit')) compatible <- TRUE
  }
  # browser()
  if (!compatible) stop("This function only works for logistic regression.")
  method <- match.arg(method)
  if (flextable) sstable <- TRUE
  if (sstable) verbose <- FALSE

  summary_method <- switch(method, lik.ratio = ._lik.ratio_summary, wald = ._wald_summary)
  result_obj <- summary_method(fit, stat_digits = stat_digits, p_digits = p_digits, verbose = verbose)

  out <- structure(result_obj$table, footer = result_obj$footer, method = method, class = c('logist_summary','data.frame'))
  if (sstable) return(as_sstable.logist_summary(out, flextable = flextable))
  if (verbose) return(invisible(print(out)))
  return(out)
}

._wald_summary <- function(fit, stat_digits=2, p_digits=4, verbose = FALSE){
  est <- coef(fit)
  ci <- suppressMessages(confint.default(fit))
  result <- data.frame(log.OR = est, OR = exp(est),
                       lower.CI = exp(ci[,1]), upper.CI = exp(ci[,2]), p.value = summary(fit)$coef[,"Pr(>|z|)"])
  for (i in 1:4) result[,i] <- formatC(result[,i], digits = stat_digits, format = 'f')
  if (length(result$p.value)) result$p.value <- formatC(result$p.value, p_digits, format = 'f')
  if (naprint(fit$na.action)!="") message("\nNote:",naprint(fit$na.action))
  if (verbose){
    cat("\nNote: 95% confidence intervals and p-values are based on Wald statistics.\n\n",
        "\n(Inference based on likelihood ratio statistics can be obtained with logist.summary(",deparse(substitute(obj)),") and is usually more accurate.\n)",sep="")
  }
  return(list(table = result,
              footer = "Note: 95% confidence intervals and p-values are based on Wald statistics."))
}

._lik.ratio_summary <- function(fit, stat_digits=2, p_digits=4, verbose = FALSE, sstable = FALSE){
  est <- coef(fit)
  ci <- suppressMessages(confint(fit))
  result <- data.frame(log.OR = est, OR = exp(est),
                       lower.CI = exp(ci[,1]), upper.CI = exp(ci[,2]))

  has.intercept <- (names(coef(fit))[1]=="(Intercept)")
  any.ia <- any(attr(terms(formula(fit)),"order")>1)
  # browser()

  if (has.intercept)
    if (any.ia){
    explicit_form <- ._explicit_lm(fit, meta = TRUE)
    explicit_model <- explicit_form$model
    explicit_meta <- explicit_form$meta
    lr.test <- data.frame(drop1(explicit_model, test='Chisq'))[-1,c("Df","Pr..Chi.")]
    rownames(lr.test) <- dplyr::recode(rownames(lr.test), !!!explicit_meta$names)
    p.value <-  lr.test[,"Pr..Chi.", drop = FALSE]
    names(p.value) <- 'p.value'
    row_order <- factor(rownames(result), levels = rownames(result))
    result <- merge(result, p.value, all.x = TRUE, by='row.names')
    result$Row.names <- factor(result$Row.names, levels = row_order)
    result <- dplyr::arrange(result, Row.names)
    rownames(result) <- result$Row.names
    result <- result[, -1]
  } else {
    lr.test <- data.frame(drop1(fit,test="Chisq"))[-1,c("Df","Pr..Chi.")]
    result$p.value <- c(NA,lr.test[,"Pr..Chi."])
  }

  for (i in 1:4) result[,i] <- formatC(result[,i], digits = stat_digits, format = 'f')
  if (length(result$p.value)) result$p.value <- formatC(result$p.value, p_digits, format = 'f')
  result$p.value <- ifelse(grepl('NA', result$p.value), '', result$p.value)
  if (naprint(fit$na.action)!="") message("\nNote:",naprint(fit$na.action))
  if (verbose) {
    cat("\nNote: 95% confidence intervals and p-values are based on likelihood ratio statistics.\n\n")
    if (!has.intercept) cat("\nNote: p-values of LR-tests not calculated because model has no intercept.\n\n")
  }

  return(list(table = result,
              footer = c(
                "Note: 95% confidence intervals and p-values are based on likelihood ratio statistics.",
                if (!has.intercept)
                  "p-values of LR-tests not calculated because model has no intercept.")))
  # return(result)
}

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
explicit.lm <- function(fit, data = NULL){
  new_fit <- ._explicit_lm(fit, data = data, meta = FALSE)
  if (length(data)) new_fit$call$data <- rlang::sym(deparse(substitute(data)))
  new_fit
}


._explicit_lm <- function(fit, data = NULL,  meta = FALSE){
  # browser()
  ia <- ._get_interaction_terms(fit)
  ia.vars <- ia$ia.vars
  ia.terms <- ia$ia.terms
  fit.term_labels <- attr(terms(formula(fit)),"term.labels")
  if (!length(fit$data) & !length(fit$model) & !length(data))
    stop('No data found within fit. Please specify data!')
  # fit_data <- model.frame(fit)
  # fit_data <- merge(as.data.frame(eval(attr(terms(fit), 'variables'))), as.data.frame(fit$model), all.x = TRUE, all.y = TRUE)
  # fit_data <- merge(as.data.frame(fit$data), as.data.frame(fit$model), all.x = TRUE, all.y = TRUE)
  fit_data <-
    if (length(data))
      as.environment(merge(as.data.frame(data), as.data.frame(fit$model), all.x = TRUE, all.y = TRUE))
    else if (is.environment(fit$data))
      as.environment(c(as.list(fit$data), as.list(fit$model)))
    else
      as.environment(merge(as.data.frame(fit$data), as.data.frame(fit$model), all.x = TRUE, all.y = TRUE))


  nonia.vars <- unique(fit.term_labels[!fit.term_labels %in% unlist(c(ia.vars, ia.terms))])

  # browser()
  var.classes <- attr(terms(fit), 'dataClasses')
  ia.classes <- var.classes[unique(unlist(ia.vars))]
  dummy_vars <- ._create_dummy(fit_data, names(var.classes)[!var.classes %in% c('numeric', 'other')])
  dummy_cols <- purrr::flatten(dummy_vars)
  dummy_cols.names <- sapply(dummy_vars, names, simplify = FALSE)
  explicit_data <- if (length(dummy_cols)) as.environment(c(as.list(fit_data), as.list(dummy_cols))) else fit_data

  ._quote_vars <- function(vars){
    sapply(vars,
           function(.vars){
             sapply(.vars,
                    function(.var){
                      paste0('`',.var,'`')
                    }, simplify = FALSE)
           }, simplify = FALSE)
  }
  # browser()
  ia.vars_dummy <-
    sapply(ia.vars,
           function(ia.var){
             sapply(ia.var,
                    function(iv){
                      if (iv %in% names(dummy_cols.names))
                        dummy_cols.names[[iv]]
                      else
                        iv
                    }, simplify = FALSE)
           }, simplify = FALSE)

  ia.vars_dummy.quoted <- ._quote_vars(ia.vars_dummy)

  nonia.vars_dummy <-
    sapply(nonia.vars,
           function(nonia.var){
             sapply(nonia.var,
                    function(niv){
                      if (niv %in% names(dummy_cols.names))
                       dummy_cols.names[[niv]]
                      else
                        niv
                    }, simplify = FALSE)
           }, simplify = FALSE)

  nonia.vars_dummy.quoted <- ._quote_vars(nonia.vars_dummy)

  ia.vars_dummy.interactTerms <-
    sapply(ia.vars_dummy,
           function(ia.var){
             expand_matrix <- expand.grid(ia.var)
             return(paste0('I(', paste(expand_matrix[,1], expand_matrix[,2], sep = ' * '), ')'))
           }, simplify = FALSE)

  ia.vars_dummy.interactTerms.quoted <-
    sapply(ia.vars_dummy.quoted,
           function(ia.var){
             expand_matrix <- expand.grid(ia.var)
             return(paste0('I(', paste(expand_matrix[,1], expand_matrix[,2], sep = ' * '), ')'))
           }, simplify = FALSE)

  ia.vars_dummy.allTerms <-
    sapply(names(ia.vars_dummy.quoted),
           function(i) c(ia.vars_dummy.quoted[[i]], interaction = ia.vars_dummy.interactTerms.quoted[[i]]),
           simplify = FALSE)

  vars_dummy.allTerms <- c(nonia.vars_dummy.quoted, ia.vars_dummy.allTerms)

  # browser()
  all_terms.fml <- as.formula(
    paste0('~',
           paste(unique(unlist(vars_dummy.allTerms)), collapse = '+'))
  )

  parent.env(explicit_data) <- parent.frame()
  # assign(deparse(fit$call$data), explicit_data)
  # explicit_model <- get(deparse(fit$call$data)) %>% update(fit, all_terms.fml, data = .)
  explicit_model <- update(fit, all_terms.fml, data = explicit_data)
  explicit_model$call$data <- fit$call$data

  if (meta){
    # browser()
    ia.vars_dummy.interactTerms.beautified <-
      sapply(ia.vars_dummy,
             function(ia.var){
               expand_matrix <- expand.grid(ia.var)
               return(paste(expand_matrix[,1], expand_matrix[,2], sep = ':'))
             }, simplify = FALSE)

    ia_vars.names <-
      unlist(lapply(names(ia.vars_dummy),
                    function(i) c(ia.vars_dummy[[i]], interaction = ia.vars_dummy.interactTerms.beautified[[i]])
      ))

    all_var.names <- unlist(c(nonia.vars_dummy, ia_vars.names))
    names(all_var.names) <- unlist(attr(terms(all_terms.fml), 'term.labels'))
  }

  # browser()

  if (meta) return(list(model = explicit_model, meta = list(names = all_var.names)))
  return(explicit_model)
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

._create_dummy <- function(data, cols){
  # browser()
  sapply(
    cols,
    function(.col){
      col_data <- as.factor(data[[.col]])
      col_value <- levels(col_data)[-1]
      new_cols <-
        lapply(col_value,
               function(val){
                 as.numeric(col_data == val)
               })
      names(new_cols) <- paste0(.col, col_value)
      return(new_cols)
    }, simplify = FALSE
  )
}

