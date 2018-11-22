
# extract x variables from formula for sstable ----------------------------

getxs <- function(xformula) {
  if (!is.call(xformula)) {
    deparse(xformula)
  } else {
    if (identical(xformula[[1]], quote(`+`))) {
      unlist(lapply(c(xformula[[2]], xformula[[3]]), getxs))
    } else {
      deparse(xformula)
    }
  }
}


# extract information from formula for sstable ----------------------------

sstable.formula <- function(formula) {
  if (length(formula) < 3) {
    stop("Missing row-wise variable(s) !!!")
  }

  ## get formula to extract all required variables (formula0)
  formula0 <- if (identical(formula[[3]], 1)) {
    as.formula(paste("~", deparse(formula[[2]])))
  } else {
    as.formula(paste("~", gsub(pattern = "[~|]", replacement = "+", x = deparse(formula))))
  }


  ## get index of each element in formula
  xlen <- length(all.vars(formula[[2]]))
  if (xlen == 0) {stop("Missing row-wise variable(s) !!!")}

  if (length(formula[[3]]) > 1) {
    if (deparse(formula[[3]][[1]]) == "|") {
      ylen <- ifelse(identical(formula[[3]][[2]], 1), 0, length(all.vars(formula[[3]][[2]])))
      zlen <- length(all.vars(formula[[3]][[3]]))
    } else {
      ylen <- ifelse(identical(formula[[3]], 1), 0, length(all.vars(formula[[3]])))
      zlen <- 0
    }
  } else {
    ylen <- ifelse(identical(formula[[3]], 1), 0, length(all.vars(formula[[3]])))
    zlen <- 0
  }

  if (ylen > 1) {stop("Only 1 column-wise variable is allowed !!!")}
  if (zlen > 1) {stop("Only 1 third dimension variable is allowed !!!")}

  ## get all formulas required for tabulation
  xs <- getxs(formula[[2]])
  formula1 <- sapply(xs, function(x) update.formula(old = formula, new = paste(x, " ~ .")))

  ## output
  return(list(formula0 = formula0,
              index = list(x = 1:xlen,
                           y = if (ylen == 0) 0 else (xlen + 1),
                           z = if (zlen == 0) 0 else (xlen + 2)),
              formula1 = formula1))
}


# calculate summary statistics for continuous variable --------------------

#' Calculate summary statistics for continuous variable
#'
#' @description A function to calculate summary statistics for continuous variable.
#'
#' @param x a numeric vector to be summarised.
#' @param statistics a character specifies summary statistics for continuous row variables.
#' @param digits a number specifies number of significant digits for numeric statistics.
#' @param n a logical value specifies whether output will contain the number of non-missing values.
#'
#' @return a string displays formatted summary statistics.
#' @export
contSummary <- function(x, statistics = c("med.IQR", "med.90", "med.range", "mean.sd"), digits = 1, n = TRUE){
  if (length(statistics) > 1) statistics <- "med.IQR"
  loc <- formatC(ifelse(statistics == "mean.sd",
                        mean(x, na.rm = TRUE),
                        median(x, na.rm = TRUE)), digits = digits, format = "f")
  dis <- switch(statistics,
                mean.sd   = formatC(sd(x, na.rm = TRUE), digits = digits, format = "f"),
                med.IQR   = paste(
                  formatC(quantile(x, probs = c(0.25, 0.75), na.rm = TRUE), digits = digits, format = "f"),
                  collapse = ", "),
                med.90    = paste(
                  formatC(quantile(x, probs = c(0.05, 0.95), na.rm = TRUE), digits = digits, format = "f"),
                  collapse = ", "),
                med.range = paste(
                  formatC(quantile(x, probs = c(0.00, 1.00), na.rm = TRUE), digits = digits, format = "f"),
                  collapse = ", "))
  if (n == TRUE){
    output <- paste(loc, " (", dis, ") - ", length(na.omit(x)), sep = "")
    names(output) <- paste(statistics, "- n")
  } else {
    output <- paste(loc, " (", dis, ")", sep = "")
    names(output) <- statistics
  }
  return(output)
}

# create baseline table ---------------------------------------------------

#' Create baseline table
#'
#' @description A function to create a simple summary baseline table.
#'
#' @param formula a formula specifies variables for rows, variable for column and third-dimension variable.
#' @param data a data frame to derive baseline table from.
#' @param bycol a logical value specifies whether the summary will be by column or by row.
#' @param pooledGroup a logical value specifies whether to pool all subgroups of column variable.
#' @param statistics a character specifies summary statistics for continuous row variables.
#' @param continuous a logical vector specifies whether each row variables is continuous or categorical.
#' @param digits a number specifies number of significant digits for numeric statistics.
#' @param test a logical value specifies whether a statistical test will be performed to compare between treatment arms.
#' @param pdigits a number specifies number of significant digits for p value.
#' @param pcutoff a number specifies threshold value of p value to be displayed as "< pcutoff".
#' @param chisq.test a logical value specifies whether Chi-squared test or Fisher's exact test will be used to compare between treatment arms.
#' @param correct a parameter for chisq.test().
#' @param simulate.p.value a parameter for chisq.test() and fisher.test().
#' @param B a parameter for chisq.test() and fisher.test().
#' @param workspace a parameter for fisher.test().
#' @param hybrid a parameter for fisher.test().
#' @param footer a vector of strings to be used as footnote of table.
#' @param flextable a logical value specifies whether output will be a flextable-type table.
#' @param bg a character specifies color of the odd rows in the body of flextable-type table.
#'
#' @return a flextable-type table or a list with values/headers/footers
#' @export
sstable.baseline <- function(formula, data, bycol = TRUE, pooledGroup = FALSE,
                             statistics = "med.IQR", continuous = NA, digits = 1,
                             test = FALSE, pdigits = 3, pcutoff = 0.0001,
                             chisq.test = FALSE, correct = FALSE, simulate.p.value = FALSE, B = 2000,
                             workspace = 1000000, hybrid = FALSE,
                             footer = NULL, flextable = FALSE, bg = "#F2EFEE") {

  ## get information from formula
  info <- sstable.formula(formula)

  ## get data
  dat <- model.frame(info$formula0, data = data, na.action = NULL)
  x <- dat[, info$index$x, drop = FALSE]
  y <- if (info$index$y > 0) dat[, info$index$y] else NULL
  z <- if (info$index$z > 0) dat[, info$index$z] else NULL
  browser()

  ## y must be categorical variable
  if (!is.null(y)) {
    if (all(!c("character", "factor", "logical") %in% class(y)) |
        (any(c("numeric", "integer") %in% class(y)) & length(unique(na.omit(y))) > 5)) {
      stop("Colum-wise variable must be categorical !!!")
    }
    y <- factor(as.character(y), levels = sort(unique(as.character(y)), na.last = TRUE), exclude = NULL)
  } else {
    y <- factor(rep("Total", nrow(dat)))
  }

  #browser()

  ## determine type of x (argument: continuous)
  if (length(continuous) == 1) {
    continuous <- rep(continuous, ncol(x))
  } else {
    if (length(continuous) != ncol(x)) stop("continuous argument must have length 1 or similar length as number of row-wise variables !!!")
  }

  continuous <- sapply(1:ncol(x), function(i) {
    out <- ifelse(is.na(continuous[i]),
                  ifelse(any(c("factor", "character", "logical") %in% class(x[, i])) |
                           (any(c("numeric", "integer") %in% class(x[, i])) & length(unique(na.omit(x[, i]))) <= 5), FALSE, TRUE),
                  continuous[i])
    return(out)
  })


  for (i in (1:ncol(x))) {
    if (continuous[i] == FALSE & !is.factor(x[, i])) x[, i] <- factor(x[, i], levels = sort(unique(na.omit(x[, i]))))
  }

  ## if z exists, x must be categorical
  if (!is.null(z) & any(continuous == TRUE)) stop("Row-wise variable must be categorical when third dimension variable exists !!!")

  ## if use by-row layout, x must be categorical
  #browser()
  if (bycol == FALSE & any(sapply(1:ncol(x), function(i) is.factor(x[,i])) == FALSE)) stop("Row-wise variable must be categorical in by-row layout !!!")

  ## determine type of z
  if (!is.null(z)) {
    zcontinuous <- ifelse(any(c("factor", "character", "logical") %in% class(z)) |
                            (any(c("numeric", "integer") %in% class(z)) & length(unique(na.omit(z))) <= 5), FALSE, TRUE)
    if (zcontinuous == FALSE & !is.factor(z)) z <- factor(z, levels = unique(na.omit(z)))
  }

  ## digits
  if (length(digits) == 1) {
    digits <- rep(digits, ncol(x))
  } else {
    if (length(digits) != ncol(x)) stop("digits argument must have length 1 or similar length as number of row-wise variables !!!")
  }



  ## if pooledGroup
  if (pooledGroup) {
    x <- rbind(x, x)
    ypool <- ifelse("total" %in% tolower(levels(y)), "pooledGroup", "Total")
    y <- factor(c(as.character(y), rep(ypool, nrow(dat))), levels = c(levels(y), ypool), exclude = NULL)
    z <- if (!is.null(z)) factor(c(z, z), levels = unique(na.omit(z))) else NULL
  }

  ## get variable name
  varname <- getlabel(x)

  ## get summary
  value <- do.call(rbind,
                   lapply(1:ncol(x), function(i) {
                     sstable.baseline.each(varname = varname[i],
                                           x = x[, i], y = y, z = z, bycol = bycol,
                                           pooledGroup = pooledGroup, statistics = statistics,
                                           continuous = continuous[i], test = test,
                                           digits = digits[i], pdigits = pdigits, pcutoff = pcutoff,
                                           chisq.test = chisq.test, correct = correct,
                                           workspace = workspace, hybrid = hybrid,
                                           simulate.p.value = simulate.p.value, B = B)
                   }))

  ## indication of unestimatable values
  value_dim <- dim(value)
  value <- apply(value, 2, function(x) ifelse(is.na(x) | x %in% c("NA (NA, NA)", "0/0 (NaN%)"), "-", x))
  dim(value) <- value_dim

  ## output
  ### header
  gr.lev <- levels(y)
  header1 <- c("", c(rbind(rep("", length(gr.lev)), paste(gr.lev, " (N=", table(y), ")", sep = ""))))
  header2 <- c("Characteristic", rep(c("n", "Summary statistic"), length(gr.lev)))
  if (test) {
    header1 <- c(header1, "p value")
    header2 <- c(header2, "")
  }

  ### footer
  footer1 <- footer2 <- footer3 <- footer4 <- footer.con <- footer.cat <- NULL

  #### n
  footer1 <- "N is number of all patients, n is number of patients with non-missing value."

  #### summary statistics
  if ((is.null(z) & any(continuous)) | (!is.null(z) & !is.factor(z))) {
    footer.con <- paste0(switch(statistics,
                                med.IQR = "median (IQR)",
                                med.90  = "median (90% range)",
                                med.range = "median (range)",
                                mean.sd = "mean (sd)"),
                         " for continuous variable(s).")
  }
  if ((is.null(z) & any(continuous == FALSE)) | (!is.null(z) & is.factor(z))) {
    footer.cat <- paste0("absolute count (%) for categorical variable(s)")
  }
  footer2.after <- if (is.null(footer.con)) {
    paste0(footer.cat, ".")
  } else {
    if (is.null(footer.cat)) {
      footer.con
    } else {
      paste0(footer.cat, " and ", footer.con)
    }
  }
  footer2 <- paste("Summary statistic is", footer2.after)

  #### unestimatable values
  if (any(value == "-")) {
    footer3 <- "- : value cannot be estimated."
  }

  #### test
  if (test) {
    footer4.cat <- paste(ifelse(chisq.test == FALSE, "Fisher's exact test", "Chi-squared test"), "for categorical variable(s)")
    footer4.con <- "Kruskal-Wallis/Mann-Whitney U-test for continuous variable(s)."
    footer4.after <- if (is.null(footer.con)) {
      paste0(footer4.cat, ".")
    } else {
      if (is.null(footer.cat)) {
        footer4.con
      } else {
        paste0(footer4.cat, " and ", footer4.con)
      }
    }
    footer4 <- paste("p-values were based on", footer4.after)
  }

  ### flextable
  if (flextable) {
    requireNamespace("flextable")
    requireNamespace("officer")

    ## main table
    tab <- flextable::flextable(as.data.frame(value))

    ## header
    header1[1] <- header2[1]
    header1[seq(from = 2, to = 2 * length(gr.lev), by = 2)] <- header1[seq(from = 2, to = 2 * length(gr.lev), by = 2) + 1]
    if (test) header2[length(header1)] <- header1[length(header1)]

    assign("tab",
           eval(parse(text = paste0("flextable::set_header_labels(tab,",
                                    paste(paste0("V", 1:length(header1)), paste0("'", header1, "'"), sep = "=", collapse = ","),
                                    ")"))))
    assign("tab",
           eval(parse(text = paste0("flextable::add_header(tab,",
                                    paste(paste0("V", 1:length(header1)), paste0("'", header2, "'"), sep = "=", collapse = ","),
                                    ", top = FALSE)"))))
    tab <- flextable::merge_h(tab, part = "header")
    tab <- flextable::merge_v(tab, part = "header")

    ## footer
    tab <- flextable::add_footer(tab, V1 = footer1)
    tab <- flextable::merge_at(tab, i = 1, j = 1:length(header1), part = "footer")
    tab <- flextable::add_footer(tab, V1 = footer2, top = FALSE)
    tab <- flextable::merge_at(tab, i = 2, j = 1:length(header1), part = "footer")
    if (!is.null(footer3)) {
      tab <- flextable::add_footer(tab, V1 = footer3, top = FALSE)
      tab <- flextable::merge_at(tab, i = 3, j = 1:length(header1), part = "footer")
    }
    if (!is.null(footer4)) {
      tab <- flextable::add_footer(tab, V1 = footer4, top = FALSE)
      tab <- flextable::merge_at(tab, i = 3 + as.numeric(!is.null(footer3)), j = 1:length(header1), part = "footer")
    }
    if (!is.null(footer)) {
      for (k in (1:length(footer))) {
        tab <- flextable::add_footer(tab, V1 = footer[k], top = FALSE)
        tab <- flextable::merge_at(tab, i = 2 + as.numeric(!is.null(footer3)) + as.numeric(!is.null(footer4)) + k, j = 1:length(header1), part = "footer")
      }
    }

    ## format
    ### width
    tab <- flextable::autofit(tab)
    ### alignment
    tab <- flextable::align(tab, j = 1, align = "left", part = "all")
    ### faces of header
    tab <- flextable::bold(tab, part = "header")
    ### background
    tab <- flextable::bg(tab, i = seq(from = 1, to = nrow(value), by = 2), j = 1:length(header1), bg = bg, part = "body")
    ### border
    tabbd <- officer::fp_border(color="black", width = 1.5)
    tab <- border_remove(tab)
    tab <- hline(tab, border = tabbd, part = "header")
    tab <- hline_top(tab, border = tabbd, part = "all")
    tab <- hline_bottom(tab, border = tabbd, part = "body")

  } else {
    tab <- list(value = value,
                header = list(header1, header2),
                footer = c(footer1, footer2, footer3, footer4, footer),
                table = rbind(header1, header2, value))
  }
  ## output
  return(tab)
}

sstable.baseline.each <- function(varname, x, y, z, bycol = TRUE, pooledGroup = FALSE,
                                  statistics = "med.IQR", continuous = NA, test = FALSE,
                                  digits = 1, pdigits = pdigits, pcutoff = 0.0001, chisq.test = FALSE, correct = FALSE, workspace = 1000000,
                                  hybrid = FALSE, simulate.p.value = FALSE, B = 2000) {

  ## functions
  mycont.summary <- function(x, y, z) {
    ngroup <- length(levels(y))

    if (is.null(z)) {
      summarystat.nice <- by(unclass(x), y, contSummary, statistics = statistics, digits = digits, n = FALSE)
      #n <- c(by(x, y, function(x) length(na.omit(x))))
      n <- table(y[!is.na(x)])

      result <- matrix("", ncol = ngroup * 2 + 1, nrow = 1)
      result[1, seq(2, ncol(result), by = 2)] <- n
      result[1, seq(3, ncol(result), by = 2)] <- unlist(summarystat.nice)

    } else {
      summarystat.nice <- by(unclass(z), list(x, y), contSummary, statistics = statistics, digits = digits, n = FALSE)
      n <- table(x, y)
      result <- matrix("", ncol = ngroup * 2 + 1, nrow = length(levels(x)) + 1)
      result[1, seq(2, ncol(result), by = 2)] <- apply(n, 2, sum)
      result[2:nrow(result), seq(2, ncol(result), by = 2)] <- n
      result[2:nrow(result), 1] <- paste0("- ", levels(x), " (n = ", apply(n, 1, sum), ")")
      result[2:nrow(result), seq(3, ncol(result), by = 2)] <- unlist(summarystat.nice)
    }

    if (test == TRUE & ngroup > 1) {
      # overall Kruskal-Wallis test for group differences
      if (is.null(z)) {
        m <- 1:(length(x) * (1 - 0.5 * as.numeric(pooledGroup)))
        pval <- tryCatch(format.pval(kruskal.test(x = x[m], g = y[m])$p.value,
                                     eps = pcutoff, digits = pdigits, scientific = FALSE),
                         error = function(c) NA)
        result <- cbind(result, pval)
      } else {
        pval <- sapply(1:length(levels(x)), function(i) {
          q <- which(x == levels(x)[i])
          if (length(q) == 0) {
            out <- NA
          } else {
            m <- q[q %in% 1:(length(z) * (1 - 0.5 * as.numeric(pooledGroup)))]
            out <- tryCatch(format.pval(kruskal.test(x = z[m], g = y[m])$p.value,
                                        eps = pcutoff, digits = pdigits, scientific = FALSE),
                            error = function(c) NA)
          }
          return(out)
        })
        result <- cbind(result, c("", pval))
      }

    }
    return(result)
  }

  mycat.summary <- function(x, y, z) {
    ngroup <- length(levels(y))
    result <- matrix("", ncol = ngroup * 2 + 1, nrow = length(levels(x)) + 1)

    if (is.null(z)) {
      ta <- table(x, y)
      ta.prop <- if (bycol) {
        unclass(ta/rep(apply(ta, 2, sum), each = length(levels(x))))
      } else {
        unclass(ta/rep(apply(ta, 1, sum), ngroup))
      }

      ta.nice <- matrix(paste0(" (", formatC(100 * unclass(ta.prop), digits, format = "f"), "%)"),
                        nrow = nrow(ta), ncol = ncol(ta))
      result[2:nrow(result), 1] <- paste0("- ", levels(x))
      if (bycol) {
        #browser()
        result[1, seq(2, ncol(result), by = 2)] <- apply(ta, 2, sum)
        result[2:nrow(result), seq(3, ncol(result), by = 2)] <- paste0(ta, "/", rep(apply(ta, 2, sum), each = length(levels(x))), ta.nice)
      } else {
        result[2:nrow(result), 1] <- paste0(result[2:nrow(result), 1], " (n = ", apply(ta, 1, sum), ")")
        result[2:nrow(result), seq(3, ncol(result), by = 2)] <- paste0(ta, "/", rep(apply(ta, 1, sum), ngroup), ta.nice)
      }
    } else {
      tn <- table(x, y)
      tn2 <- if (pooledGroup) {tn[, -ncol(tn)]} else {tn}
      ta.nice <- matrix(by(z, list(x, y), function(z) {
        ta <- sum(z == TRUE, na.rm = TRUE)
        n <- length(na.omit(z))
        return(paste0(ta, "/", n,
                      " (", formatC(100 * (ta/n), digits, format = "f"), "%)"))
      }), ncol = ngroup)
      result[2:nrow(result), 1] <- paste0("- ", levels(x), " (n = ", apply(tn2, 1, sum), ")")
      result[2:nrow(result), seq(3, ncol(result), by = 2)] <- ta.nice
      result[1, seq(2, ncol(result), by = 2)] <- apply(tn, 2, sum)
      result[2:nrow(result), seq(2, ncol(result), by = 2)] <- tn
    }

    if (test == TRUE & ngroup > 1) {
      if (is.null(z)) {
        m <- 1:(length(x) * (1 - 0.5 * as.numeric(pooledGroup)))
        pval <- if (chisq.test) {
          tryCatch(format.pval(chisq.test(x = x[m], y = y[m], correct = correct,
                                          simulate.p.value = simulate.p.value, B = B)$p.value,
                               eps = pcutoff, digits = pdigits, scientific = FALSE),
                   error = function(c) NA)
        } else {
          tryCatch(format.pval(fisher.test(x = x[m], y = y[m], workspace = workspace, hybrid = hybrid,
                                           simulate.p.value = simulate.p.value, B = B)$p.value,
                               eps = pcutoff, digits = pdigits, scientific = FALSE),
                   error = function(c) NA)
        }
        result <- cbind(result, "")
        result[1,ngroup * 2 + 2] <- pval
      } else {
        pval <- sapply(1:length(levels(x)), function(i) {
          q <- which(x == levels(x)[i])
          if (length(q) == 0) {
            out <- NA
          } else {
            m <- q[q %in% 1:(length(z) * (1 - 0.5 * as.numeric(pooledGroup)))]
            out <- if (chisq.test) {
              tryCatch(format.pval(chisq.test(x = z[m], y = y[m], correct = correct,
                                              simulate.p.value = simulate.p.value, B = B)$p.value,
                                   eps = pcutoff, digits = pdigits, scientific = FALSE),
                       error = function(c) NA)
            } else {
              tryCatch(format.pval(fisher.test(x = z[m], y = y[m], workspace = workspace, hybrid = hybrid,
                                               simulate.p.value = simulate.p.value, B = B)$p.value,
                                   eps = pcutoff, digits = pdigits, scientific = FALSE),
                       error = function(c) NA)
            }
          }
          return(out)
        })
        result <- cbind(result, c("", pval))
      }
    }
    result
  }

  if (is.null(z)) {
    out <- if (continuous) {
      mycont.summary(x = x, y = y, z = NULL)
    } else {
      mycat.summary(x = x, y = y, z = NULL)
    }
  } else {
    out <- if (is.factor(z)) {
      mycat.summary(x = x, y = y, z = z)
    } else {
      mycont.summary(x = x, y = y, z = z)
    }
  }

  out[1, 1] <- varname
  return(out)
}

# adverse event table -----------------------------------------------------

#' Create an adverse event summary table
#'
#' @description A function to create a simple adverse event summary table.
#'
#' @param ae_data a data frame contains adverse event data.
#' @param fullid_data a data frame contains treatment arm data of all participants (not just those had adverse event).
#' @param id.var a character specifies name of study id variable (exists in both adverse event data and treatment arm data).
#' @param aetype.var a character specifies name of adverse event type variable (exists in adverse event data).
#' @param arm.var a character specifies name of treatment arm variable (exists in treatment arm data).
#' @param digits a number specifies number of significant digits for numeric statistics.
#' @param test a logical value specifies whether a statistical test will be performed to compare between treatment arms.
#' @param pdigits a number specifies number of significant digits for p value.
#' @param pcutoff a number specifies threshold value of p value to be displayed as "< pcutoff".
#' @param chisq.test a logical value specifies whether Chi-squared test or Fisher's exact test will be used to compare between treatment arms.
#' @param correct a parameter for chisq.test().
#' @param simulate.p.value a parameter for chisq.test() and fisher.test().
#' @param B a parameter for chisq.test() and fisher.test().
#' @param workspace a parameter for fisher.test().
#' @param hybrid a parameter for fisher.test().
#' @param footer a vector of strings to be used as footnote of table.
#' @param flextable a logical value specifies whether output will be a flextable-type table.
#' @param bg a character specifies color of the odd rows in the body of flextable-type table.
#'
#' @return a flextable-type table or a list with values/headers/footers
#' @import dplyr
#' @import tidyr
#' @export
sstable.ae <- function(ae_data, fullid_data, id.var, aetype.var, arm.var, digits = 0,
                       test = TRUE, pdigits = 3, pcutoff = 0.0001, chisq.test = FALSE, correct = FALSE,
                       simulate.p.value = FALSE, B = 2000, workspace = 1000000, hybrid = FALSE,
                       footer = NULL, flextable = TRUE, bg = "#F2EFEE"){
  requireNamespace("dplyr")
  requireNamespace("tidyr")

  ## check variable's name
  tmp <- match.call()
  if (!id.var %in% names(ae_data)){stop(paste(tmp[[4]], "does not exist in", deparse(tmp[[2]]), "!!!"))}
  if (!id.var %in% names(fullid_data)){stop(paste(tmp[[4]], "does not exist in", deparse(tmp[[3]]), "!!!"))}
  if (!aetype.var %in% names(ae_data)){stop(paste(tmp[[5]], "does not exist in", deparse(tmp[[2]]), "!!!"))}
  if (!arm.var %in% names(fullid_data)){stop(paste(tmp[[6]], "does not exist in", deparse(tmp[[3]]), "!!!"))}

  ## format study arms
  idarm <- fullid_data[, c(id.var, arm.var)]; colnames(idarm) <- c("id", "arm")
  arm_lev <- sort(unique(as.character(idarm$arm)), na.last = TRUE)
  idarm$arm <- with(idarm, factor(as.character(arm), levels = arm_lev, exclude = NULL))

  ## add aetype of "Any selected AE" & format aetype
  ae_any <- ae_data; ae_any[, aetype.var] <- "Any selected adverse event"
  ae <- rbind(ae_data, ae_any)[, c(id.var, aetype.var)]; colnames(ae) <- c("id", "aetype")
  ae$aetype <- addNA(factor(as.character(ae$aetype),
                            levels = c("Any selected adverse event", sort(unique(as.character(ae_data[, aetype.var])))),
                            exclude = NULL), ifany = TRUE)

  ## add randomized arm to AE
  ae_arm <- merge(idarm, ae, by = "id", all.y = TRUE)

  ## calculate episodes and patients
  ae_count <- ae_arm %>%
    group_by(aetype, arm) %>%
    summarise(n_episode = n(),
              n_patient = length(unique(id))) %>%
    ungroup() %>%
    complete(aetype, arm)

  episode_n <- unlist(spread(select(as.data.frame(ae_count), -n_patient), key = arm, value = n_episode)[, -1])
  episode_n[is.na(episode_n)] <- 0
  patient_n <- unlist(spread(select(ae_count, -n_episode), key = arm, value = n_patient)[, -1])
  patient_n[is.na(patient_n)] <- 0
  patient_N <- rep(table(idarm$arm), each = nlevels(ae_arm$aetype))
  patient_p <- patient_n/patient_N

  ## table
  value <- matrix(ncol = nlevels(ae_arm$arm) * 2, nrow = nlevels(ae_arm$aetype))
  value[, seq(from = 1, to = ncol(value), by = 2)] <- episode_n
  value[, seq(from = 2, to = ncol(value), by = 2)] <- paste0(patient_n, "/", patient_N,
                                                             " (", formatC(100 * patient_p, digits, format = "f"), "%)")
  ae_value <- cbind(levels(ae_arm$aetype), value)

  ## test
  if (test) {
    pval <- sapply(1:nrow(value), function(i) {
      idx <- seq(from = i, to = length(patient_n), by = nrow(value))
      mat <- matrix(c(patient_n[idx], patient_N[idx] - patient_n[idx]), nrow = 2, byrow = TRUE)
      out <- if (chisq.test) {
        tryCatch(format.pval(chisq.test(x = mat, correct = correct,
                                        simulate.p.value = simulate.p.value, B = B)$p.value,
                             eps = pcutoff, digits = pdigits, scientific = FALSE),
                 error = function(c) NA)
      } else {
        tryCatch(format.pval(fisher.test(x = mat, workspace = workspace, hybrid = hybrid,
                                         simulate.p.value = simulate.p.value, B = B)$p.value,
                             eps = pcutoff, digits = pdigits, scientific = FALSE),
                 error = function(c) NA)
      }
      return(out)
    })
    pval[is.na(pval)] <- "-"
    ae_value <- cbind(ae_value, pval)
  }

  ## output
  ### header
  gr.lev <- levels(ae_arm$arm)
  header1 <- c("", c(rbind(rep("", length(gr.lev)), paste(gr.lev, " (N=", table(idarm$arm), ")", sep = ""))))
  header2 <- c("Type of adverse event", rep(c("n episode", "n patient"), length(gr.lev)))
  if (test) {
    header1 <- c(header1, "p value")
    header2 <- c(header2, "")
  }

  ### footer
  footer1 <- footer2 <- footer3 <- footer4 <- NULL

  #### n
  footer1 <- "n episode refer to the number of adverse events in each study arm."
  footer2 <- "n patient refer to the number of patients with at least one event in each study arm."

  #### unestimatable values
  if (any(value == "-")) {
    footer3 <- "- : value cannot be estimated."
  }

  #### test
  if (test) {
    footer4 <- paste("p-values were based on",
                     ifelse(chisq.test == FALSE, "Fisher's exact test", "Chi-squared test"),
                     "comparing n patient between study arms for each type of adverse event.")
  }

  ### flextable
  if (flextable) {
    requireNamespace("flextable")
    requireNamespace("officer")

    ## main table
    colnames(ae_value) <- rep("", ncol(ae_value))
    tab <- flextable::flextable(as.data.frame(ae_value))

    ## header
    header1[1] <- header2[1]
    header1[seq(from = 2, to = 2 * length(gr.lev), by = 2)] <- header1[seq(from = 2, to = 2 * length(gr.lev), by = 2) + 1]

    if (test) header2[length(header1)] <- header1[length(header1)]
    assign("tab",
           eval(parse(text = paste0("flextable::set_header_labels(tab,",
                                    paste(paste0("V", 1:length(header1)), paste0("'", header1, "'"), sep = "=", collapse = ","),
                                    ")"))))
    assign("tab",
           eval(parse(text = paste0("flextable::add_header(tab,",
                                    paste(paste0("V", 1:length(header1)), paste0("'", header2, "'"), sep = "=", collapse = ","),
                                    ", top = FALSE)"))))
    tab <- flextable::merge_h(tab, part = "header")
    tab <- flextable::merge_v(tab, part = "header")

    ## footer
    tab <- flextable::add_footer(tab, V1 = footer1)
    tab <- flextable::merge_at(tab, i = 1, j = 1:length(header1), part = "footer")
    tab <- flextable::add_footer(tab, V1 = footer2, top = FALSE)
    tab <- flextable::merge_at(tab, i = 2, j = 1:length(header1), part = "footer")
    if (!is.null(footer3)) {
      tab <- flextable::add_footer(tab, V1 = footer3, top = FALSE)
      tab <- flextable::merge_at(tab, i = 3, j = 1:length(header1), part = "footer")
    }
    if (!is.null(footer4)) {
      tab <- flextable::add_footer(tab, V1 = footer4, top = FALSE)
      tab <- flextable::merge_at(tab, i = 3 + as.numeric(!is.null(footer3)), j = 1:length(header1), part = "footer")
    }
    if (!is.null(footer)) {
      for (k in (1:length(footer))) {
        tab <- flextable::add_footer(tab, V1 = footer[k], top = FALSE)
        tab <- flextable::merge_at(tab, i = 2 + as.numeric(!is.null(footer3)) + as.numeric(!is.null(footer4)) + k, j = 1:length(header1), part = "footer")
      }
    }

    ## format
    ### width
    tab <- flextable::autofit(tab)
    ### alignment
    tab <- flextable::align(tab, j = 1, align = "left", part = "all")
    ### faces of header
    tab <- flextable::bold(tab, part = "header")
    ### background
    tab <- flextable::bg(tab, i = seq(from = 1, to = nrow(value), by = 2), j = 1:length(header1), bg = bg, part = "body")
    ### border
    tabbd <- officer::fp_border(color="black", width = 1.5)
    tab <- border_remove(tab)
    tab <- hline(tab, border = tabbd, part = "header")
    tab <- hline_top(tab, border = tabbd, part = "all")
    tab <- hline_bottom(tab, border = tabbd, part = "body")

  } else {
    tab <- list(value = ae_value,
                header = list(header1, header2),
                footer = c(footer1, footer2, footer3, footer4, footer),
                table = rbind(header1, header2, ae_value))
  }
  return(tab)
}
