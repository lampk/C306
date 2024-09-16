
# extract x variables from formula for sstable ----------------------------

getvar <- function(formula) {
  if (!is.call(formula)) {
    deparse(formula)
  } else {
    if (identical(formula[[1]], quote(`~`)) | identical(formula[[1]], quote(`+`)) | identical(formula[[1]], quote(`|`))) {
      unlist(lapply(c(formula[[2]], formula[[3]]), getvar))
    } else {
      deparse(formula)
    }
  }
}

# extract the sign from quo/formula for sstable - trinhdhk ----------------

getsign <- function(fml){
  fml <- unclass(fml)[[2]]
  .getsign = function(x){
    c(x[[1]],
      unlist(lapply(x[-1],
                    function(.x) if(is.call(.x)) .getsign(.x))))
  }

  out <- .getsign(fml)
  out[length(out):1]
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
    as.formula(paste("~", paste(getvar(formula), collapse = "+")))
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
  xs <- getvar(formula[[2]])
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

contSummary <- function(x, statistics = NULL, digits = 1, n = TRUE) {
  if (is.null(statistics)) {
    stop("Please provide a string for 'statistics'")
  }

  # Convert the 'statistics' string to lowercase
  statistics <- tolower(statistics)

  # Define a list of supported statistics and their corresponding functions
  stat_list <- list(
    mean = function(x) round(mean(x, na.rm = TRUE), digits),
    median = function(x) round(median(x, na.rm = TRUE), digits),
    sd = function(x) round(sd(x, na.rm = TRUE), digits),
    q1 = function(x) round(quantile(x, 0.25, na.rm = TRUE), digits),
    q3 = function(x) round(quantile(x, 0.75, na.rm = TRUE), digits),
    min = function(x) round(min(x, na.rm = TRUE), digits),
    max = function(x) round(max(x, na.rm = TRUE), digits)
  )

  # Replace statistical names with their calculated values
  for (stat_name in names(stat_list)) {
    stat_value <- stat_list[[stat_name]](x)
    statistics <- gsub(stat_name, stat_value, statistics)
  }

  # Include the count if requested
  if (n) {
    statistics <- paste0(statistics)
  }

  return(statistics)
}

# create baseline table ---------------------------------------------------

#' Create baseline table
#'
#' @description A function to create a simple summary baseline table.
#'
#' @param formula a formula specifies variables for rows, variable for column and third-dimension variable.
#'
#' If formula is in the form of x+y ~ a, the function will summary x and y with respects to each value of a
#'
#' If formula is in the form of x+y ~ a|z, the function will summary z with respects to each value of {a,x} and {a, y}
#' @param data a data frame to derive baseline table from.
#' @param bycol a logical value specifies whether the summary will be by column or by row.
#' @param pooledGroup a logical value specifies whether to pool all subgroups of column variable.
#' @param keepEmptyGroup a logical value specifying whether should keep empty groups
#' @param statistics a character specifies summary statistics for continuous row variables.
#' @param cont a vector specifies whether each row variables is continuous.
#' @param cate a vector specifies whether each row variables is categorical.
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
#' @param df a logical values specifies whether output will be a draw dataframe.
#'
#' @return a flextable-type table or a list with values/headers/footers
#' @export
# function to format data frame to baseline table -hungtt
sstable.baseline.edit <- function(value, formula, data, bycol = TRUE, pooledGroup = FALSE, keepEmptyGroup = FALSE,
                                   statistics = "mean, median (Q1-Q3)", cont = NULL, cate = NULL, fullfreq = TRUE, digits = 1,
                                   test = FALSE, pdigits = 3, pcutoff = 0.0001,
                                   chisq.test = FALSE, correct = FALSE, simulate.p.value = FALSE, B = 2000,
                                   workspace = 1000000, hybrid = FALSE, footer = NULL, flextable = FALSE, bg = "#F2EFEE") {
  ## get information from formula
  info <- sstable.formula(formula)

  ## strip the tibble class which causes issue - trinhdhk
  data <- as.data.frame(data)

  ## get data
  dat <- model.frame(info$formula0, data = data, na.action = NULL)
  x <- xlabel <- dat[, info$index$x, drop = FALSE]
  y <- if (info$index$y > 0) dat[, info$index$y] else NULL
  z <- if (info$index$z > 0) dat[, info$index$z] else NULL

  ## y must be categorical variable
  if (!is.null(y)) {
    if (all(!c("character", "factor", "logical") %in% class(y)) |
        (any(c("numeric", "integer") %in% class(y)) & length(unique(na.omit(y))) > 5)) {
      stop("Column-wise variable must be categorical !!!")
    }
    if (is.factor(y)){
      ### [trinhdhk] 2024-04: reverse level of y for better summary
      y <- ._lv_rev_(y)
      if (!keepEmptyGroup) y <- droplevels(y)
      y <- addNA(y, ifany = TRUE)
    } else {
      y <- factor(as.character(y), levels = sort(unique(as.character(y), decreasing = TRUE), na.last = TRUE), exclude = NULL)
    }
  } else {
    y <- factor(rep("Total", nrow(dat)))
  }

  #browser()

  ## determine type of x (argument: cont and cate)
  varlist <- getvar(formula) #hungtt
  varlist <- varlist[-length(varlist)] #remove the y name

  # Convert varlist, cont, and cate to lowercase for case-insensitive comparison
  varlist_lower <- tolower(varlist)
  cont_lower <- tolower(cont)
  cate_lower <- tolower(cate)

  continuous <- ifelse(varlist_lower %in% cont_lower, TRUE, ifelse(varlist_lower %in% cate_lower, FALSE, NA)) #assign var type

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
    zdiscrete <- any(c("factor", "character", "logical") %in% class(unclass(z))) |
      (any(c("numeric", "integer") %in% class(unclass(z))) & length(unique(na.omit(z))) <= 5)
    # if (zcontinuous == FALSE & !is.factor(z)) z <- factor(z, levels = unique(na.omit(z)))
    if (zdiscrete) z <- factor(z, levels = unique(na.omit(z)))
  }

  # browser()
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
  # else{
  #   z <- if (!is.null(z)) factor(z, levels = unique(na.omit(z))) else NULL
  # }

  ## get variable name
  varname <- if (ncol(xlabel) == 1) getlabel(xlabel[, 1]) else getlabel(xlabel)



  # Initialize an empty vector to store labels or variable names
  label_list <- character(length(varlist))

  # Loop through the variables in varlist
  for (i in seq_along(varlist)) {
    var_name <- varlist[i]

    # Retrieve the variable label if available
    var_label <- attr(data[[var_name]], "label")

    # Check if the variable has a label or use the variable name
    if (!is.null(var_label) && var_label != "") {
      label_list[i] <- var_label
    } else {
      label_list[i] <- var_name
    }
  }
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
  footer2 <- footer1 <- footer1.after <- footer.con <- footer.cat <- NULL

  #### summary statistics
  if ((is.null(z) & any(continuous)) | (!is.null(z) & !is.factor(z))) {
    footer.con <- paste0(
      paste(statistics, collapse = ", "),
      " for continuous variable(s)."
    )
  }

  if ((is.null(z) & any(continuous == FALSE)) | (!is.null(z) & is.factor(z))) {
    footer.cat <- "absolute count (%) for categorical variable(s)"
  }

  footer1.after <- if (is.null(footer.con)) {
    paste0(footer.cat, ".")
  } else {
    if (is.null(footer.cat)) {
      footer.con
    } else {
      paste0(footer.cat, " and ", footer.con)
    }
  }
  footer1 <- paste("Summary statistic is", footer1.after)

  #### test
  if (test) {
    footer2.cat <- paste(
      ifelse(chisq.test == FALSE, "Fisher's exact test", "Chi-squared test"),
      "for categorical variable(s)"
    )
    footer2.con <- "Kruskal-Wallis/Mann-Whitney U-test for continuous variable(s)."
    footer2.after <- if (is.null(footer.con)) {
      paste0(footer2.cat, ".")
    } else {
      if (is.null(footer.cat)) {
        footer2.con
      } else {
        paste0(footer2.cat, " and ", footer2.con)
      }
    }
    footer2 <- paste("p-values were based on", footer2.after)
  }

  footer <- c(
    "N is number of all patients, n is number of patients with non-missing value.",
    footer1,
    if (any(value == "-")) {"- : value cannot be estimated."} else NULL,
    footer2,
    footer
  )

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
    for (k in (1:length(footer))) {
      tab <- flextable::add_footer(tab, V1 = footer[k], top = FALSE)
      tab <- flextable::merge_at(tab, i = k, j = 1:length(header1), part = "footer")
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
    tab <- flextable::border_remove(tab)
    tab <- flextable::hline(tab, border = tabbd, part = "header")
    tab <- flextable::hline_top(tab, border = tabbd, part = "all")
    tab <- flextable::hline_bottom(tab, border = tabbd, part = "body")

  } else {
    tab <- list(table = rbind(header1, header2, value),
                footer = footer)
    class(tab$table) <- c('baseline_tbl', 'ss_tbl', class(tab$table))
  }
  ## output
  if (!flextable) class(tab) <- c('ss_baseline','ss_obj')
  return(tab)
}


# create baseline table ---------------------------------------------------

#' Create baseline table
#'
#' @description A function to create a simple summary baseline table.
#'
#' @param formula a formula specifies variables for rows, variable for column and third-dimension variable.
#'
#' If formula is in the form of x+y ~ a, the function will summary x and y with respects to each value of a
#'
#' If formula is in the form of x+y ~ a|z, the function will summary z with respects to each value of {a,x} and {a, y}
#' @param data a data frame to derive baseline table from.
#' @param bycol a logical value specifies whether the summary will be by column or by row.
#' @param pooledGroup a logical value specifies whether to pool all subgroups of column variable.
#' @param keepEmptyGroup a logical value specifying whether should keep empty groups
#' @param statistics a character specifies summary statistics for continuous row variables.
#' @param cont a vector specifies whether each row variables is continuous.
#' @param cate a vector specifies whether each row variables is categorical.
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
#' @param df a logical values specifies whether output will be a draw dataframe.
#'
#' @return a flextable-type table or a list with values/headers/footers
#' @export
sstable.baseline <- function(formula, data, bycol = TRUE, pooledGroup = FALSE, keepEmptyGroup = FALSE,
                             statistics = "mean, median (Q1-Q3)", cont = NULL, cate = NULL, fullfreq = TRUE, digits = 1,
                             test = FALSE, pdigits = 3, pcutoff = 0.0001,
                             chisq.test = FALSE, correct = FALSE, simulate.p.value = FALSE, B = 2000,
                             workspace = 1000000, hybrid = FALSE,
                             footer = NULL, flextable = FALSE, bg = "#F2EFEE", df = FALSE) {

  ## get information from formula
  info <- sstable.formula(formula)

  ## strip the tibble class which causes issue - trinhdhk
  data <- as.data.frame(data)

  ## get data
  dat <- model.frame(info$formula0, data = data, na.action = NULL)
  x <- xlabel <- dat[, info$index$x, drop = FALSE]
  y <- if (info$index$y > 0) dat[, info$index$y] else NULL
  z <- if (info$index$z > 0) dat[, info$index$z] else NULL

  ## y must be categorical variable
  if (!is.null(y)) {
    if (all(!c("character", "factor", "logical") %in% class(y)) |
        (any(c("numeric", "integer") %in% class(y)) & length(unique(na.omit(y))) > 5)) {
      stop("Column-wise variable must be categorical !!!")
    }
    if (is.factor(y)){
      ### [trinhdhk] 2024-04: reverse level of y for better summary
      y <- ._lv_rev_(y)
      if (!keepEmptyGroup) y <- droplevels(y)
      y <- addNA(y, ifany = TRUE)
    } else {
      y <- factor(as.character(y), levels = sort(unique(as.character(y), decreasing = TRUE), na.last = TRUE), exclude = NULL)
    }
  } else {
    y <- factor(rep("Total", nrow(dat)))
  }

  #browser()

  ## determine type of x (argument: cont and cate)
  varlist <- getvar(formula) #hungtt
  varlist <- varlist[-length(varlist)] #remove the y name

  # Convert varlist, cont, and cate to lowercase for case-insensitive comparison
  varlist_lower <- tolower(varlist)
  cont_lower <- tolower(cont)
  cate_lower <- tolower(cate)

  continuous <- ifelse(varlist_lower %in% cont_lower, TRUE, ifelse(varlist_lower %in% cate_lower, FALSE, NA)) #assign var type

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
    zdiscrete <- any(c("factor", "character", "logical") %in% class(unclass(z))) |
      (any(c("numeric", "integer") %in% class(unclass(z))) & length(unique(na.omit(z))) <= 5)
    # if (zcontinuous == FALSE & !is.factor(z)) z <- factor(z, levels = unique(na.omit(z)))
    if (zdiscrete) z <- factor(z, levels = unique(na.omit(z)))
  }

  # browser()
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
  # else{
  #   z <- if (!is.null(z)) factor(z, levels = unique(na.omit(z))) else NULL
  # }

  ## get variable name
  varname <- if (ncol(xlabel) == 1) getlabel(xlabel[, 1]) else getlabel(xlabel)


  # Initialize an empty vector to store labels or variable names
  label_list <- character(length(varlist))

  # Loop through the variables in varlist
  for (i in seq_along(varlist)) {
    var_name <- varlist[i]

    # Retrieve the variable label if available
    var_label <- attr(data[[var_name]], "label")

    # Check if the variable has a label or use the variable name

    if (!is.null(var_label) && var_label != "") {
      label_list[i] <- var_label
    } else {
      label_list[i] <- var_name
    }
  }

  ## get summary
  value <- do.call(rbind,
                   lapply(1:ncol(x), function(i) {
                     sstable.baseline.each(varname = varname[i][[1]], label_list = label_list[i][[1]],
                                           x = x[, i], y = y, z = z, bycol = bycol,
                                           pooledGroup = pooledGroup, statistics = statistics,
                                           continuous = continuous[i], fullfreq = fullfreq, test = test,
                                           digits = digits[i], pdigits = pdigits, pcutoff = pcutoff,
                                           chisq.test = chisq.test, correct = correct,
                                           workspace = workspace, hybrid = hybrid,
                                           simulate.p.value = simulate.p.value, B = B)
                   }))

  ## indication of unestimatable values
  value_dim <- dim(value)
  value <- apply(value, 2, function(x) ifelse(is.na(x) | x %in% c("NA (NA, NA)", "0/0 (NaN%)"), "-", x))
  dim(value) <- value_dim
  value <- as.data.frame(value)
  # ## output - old code, if want to reverse to old code, uncomment this code, remove df argument in main function
  #and delete the corresponding code below
  # ### header
  #
  # gr.lev <- levels(y)
  # header1 <- c("", c(rbind(rep("", length(gr.lev)), paste(gr.lev, " (N=", table(y), ")", sep = ""))))
  # header2 <- c("Characteristic", rep(c("n", "Summary statistic"), length(gr.lev)))
  # if (test) {
  #   header1 <- c(header1, "p value")
  #   header2 <- c(header2, "")
  # }
  #
  # ### footer
  # footer2 <- footer1 <- footer1.after <- footer.con <- footer.cat <- NULL
  #
  # #### summary statistics
  # if ((is.null(z) & any(continuous)) | (!is.null(z) & !is.factor(z))) {
  #   footer.con <- paste0(
  #     paste(statistics, collapse = ", "),
  #     " for continuous variable(s)."
  #   )
  # }
  #
  # if ((is.null(z) & any(continuous == FALSE)) | (!is.null(z) & is.factor(z))) {
  #   footer.cat <- "absolute count (%) for categorical variable(s)"
  # }
  #
  # footer1.after <- if (is.null(footer.con)) {
  #   paste0(footer.cat, ".")
  # } else {
  #   if (is.null(footer.cat)) {
  #     footer.con
  #   } else {
  #     paste0(footer.cat, " and ", footer.con)
  #   }
  # }
  # footer1 <- paste("Summary statistic is", footer1.after)
  #
  # #### test
  # if (test) {
  #   footer2.cat <- paste(
  #     ifelse(chisq.test == FALSE, "Fisher's exact test", "Chi-squared test"),
  #     "for categorical variable(s)"
  #   )
  #   footer2.con <- "Kruskal-Wallis/Mann-Whitney U-test for continuous variable(s)."
  #   footer2.after <- if (is.null(footer.con)) {
  #     paste0(footer2.cat, ".")
  #   } else {
  #     if (is.null(footer.cat)) {
  #       footer2.con
  #     } else {
  #       paste0(footer2.cat, " and ", footer2.con)
  #     }
  #   }
  #   footer2 <- paste("p-values were based on", footer2.after)
  # }
  #
  # footer <- c(
  #   "N is number of all patients, n is number of patients with non-missing value.",
  #   footer1,
  #   if (any(value == "-")) {"- : value cannot be estimated."} else NULL,
  #   footer2,
  #   footer
  # )
  #
  #
  # ### flextable
  # if (flextable) {
  #   requireNamespace("flextable")
  #   requireNamespace("officer")
  #
  #   ## main table
  #   tab <- flextable::flextable(as.data.frame(value))
  #
  #   ## header
  #   header1[1] <- header2[1]
  #   header1[seq(from = 2, to = 2 * length(gr.lev), by = 2)] <- header1[seq(from = 2, to = 2 * length(gr.lev), by = 2) + 1]
  #   if (test) header2[length(header1)] <- header1[length(header1)]
  #   assign("tab",
  #          eval(parse(text = paste0("flextable::set_header_labels(tab,",
  #                                   paste(paste0("V", 1:length(header1)), paste0("'", header1, "'"), sep = "=", collapse = ","),
  #                                   ")"))))
  #   assign("tab",
  #          eval(parse(text = paste0("flextable::add_header(tab,",
  #                                   paste(paste0("V", 1:length(header1)), paste0("'", header2, "'"), sep = "=", collapse = ","),
  #                                   ", top = FALSE)"))))
  #
  #   tab <- flextable::merge_h(tab, part = "header")
  #   tab <- flextable::merge_v(tab, part = "header")
  #
  #   ## footer
  #   for (k in (1:length(footer))) {
  #     tab <- flextable::add_footer(tab, V1 = footer[k], top = FALSE)
  #     tab <- flextable::merge_at(tab, i = k, j = 1:length(header1), part = "footer")
  #   }
  #
  #   ## format
  #   ### width
  #   tab <- flextable::autofit(tab)
  #   ### alignment
  #   tab <- flextable::align(tab, j = 1, align = "left", part = "all")
  #   ### faces of header
  #   tab <- flextable::bold(tab, part = "header")
  #   ### background
  #   tab <- flextable::bg(tab, i = seq(from = 1, to = nrow(value), by = 2), j = 1:length(header1), bg = bg, part = "body")
  #   ### border
  #   tabbd <- officer::fp_border(color="black", width = 1.5)
  #   tab <- flextable::border_remove(tab)
  #   tab <- flextable::hline(tab, border = tabbd, part = "header")
  #   tab <- flextable::hline_top(tab, border = tabbd, part = "all")
  #   tab <- flextable::hline_bottom(tab, border = tabbd, part = "body")
  #
  # } else {
  #   tab <- list(table = rbind(header1, header2, value),
  #               footer = footer)
  #   class(tab$table) <- c('baseline_tbl', 'ss_tbl', class(tab$table))
  # }
  # ## output
  # if (!flextable) class(tab) <- c('ss_baseline','ss_obj')


  # add option to call dataframe before adding header, footer -hungtt
if (df) { tab <- value}
else { tab <- sstable.baseline.edit(value = value, formula = formula, data =data, bycol = bycol, pooledGroup = pooledGroup , keepEmptyGroup = keepEmptyGroup,
                                     statistics = statistics, cont = cont, cate = cate, fullfreq = fullfreq, digits = digits,
                                     test = test, pdigits = pdigits, pcutoff = pcutoff,
                                     chisq.test = chisq.test, correct = correct, simulate.p.value = simulate.p.value, B = B,
                                     workspace = workspace, hybrid = hybrid, footer = footer, flextable = flextable, bg = bg)
}

return (tab)
}


sstable.baseline.each <- function(varname, label_list, x, y, z, bycol = TRUE, pooledGroup = FALSE,
                                  statistics = "mean, median (Q1-Q3)", continuous = NA, fullfreq = TRUE, test = FALSE,
                                  digits = 1, pdigits = pdigits, pcutoff = 0.0001, chisq.test = FALSE, correct = FALSE, workspace = 1000000,
                                  hybrid = FALSE, simulate.p.value = FALSE, B = 2000) {

  # ## functions to make the summary value of different statistics in different rows -hungtt
  # mycont.summary <- function(x, y, z) {
  #   ngroup <- length(levels(y))
  #
  #   if (is.null(z)) {
  #     summarystat.nice <- by(unclass(x), y, contSummary, statistics = statistics, digits = digits, n = FALSE)
  #     n <- table(y[!is.na(x)])
  #
  #     # Determine the number of values in each element of summarystat.nice
  #     n_values <- length(summarystat.nice[[1]])
  #
  #     # Create result matrix with appropriate dimensions
  #     result <- matrix("", nrow = n_values, ncol = 1 + 2 * ngroup)
  #
  #
  #     # Fill in the n values and summarystat.nice values
  #     for (i in 1:ngroup) {
  #       result[, 2 * i] <- n[i]
  #       for (j in 1:n_values) {
  #         result[j, 2 * i + 1] <- summarystat.nice[[i]][j]
  #       }
  #     }
  #
  #   } else {
  #     summarystat.nice <- by(unclass(z), list(x, y), contSummary, statistics = statistics, digits = digits, n = FALSE)
  #     n <- table(x, y)
  #
  #     # Determine the number of values in each element of summarystat.nice
  #     n_values <- length(summarystat.nice[[1]])
  #
  #     # Create result matrix with appropriate dimensions
  #     result <- matrix("", nrow = n_values, ncol = 1 + 2 * ngroup)
  #
  #     # Fill in the first column with labels
  #     result[, 1] <- paste0("- ", levels(x))
  #
  #     # Fill in the n values and summarystat.nice values
  #     for (i in 1:ngroup) {
  #       result[, 2 * i] <- apply(n, 1, sum)[i]
  #       for (j in 1:n_values) {
  #         result[j, 2 * i + 1] <- summarystat.nice[[i]][j]
  #       }
  #     }
  #   }

  mycont.summary <- function(x, y, z) {
    ngroup <- length(levels(y))

    if (is.null(z)) {
      summarystat.nice <- by(unclass(x), y, contSummary, statistics = statistics, digits = digits, n = FALSE)
      n <- table(y[!is.na(x)])

      result <- matrix("", ncol = ngroup * 2 + 1, nrow = 1)
      result[1, seq(2, ncol(result), by = 2)] <- n

      # Check if summarystat.nice has more than one value per row and flatten it if necessary
      if (length(summarystat.nice) > 1) {
        result[1, seq(3, ncol(result), by = 2)] <- unlist(lapply(summarystat.nice, function(x) paste(x, collapse = ", ")))
      } else {
        result[1, seq(3, ncol(result), by = 2)] <- unlist(summarystat.nice)
      }

    } else {
      summarystat.nice <- by(unclass(z), list(x, y), contSummary, statistics = statistics, digits = digits, n = FALSE)
      n <- table(x, y)

      result <- matrix("", ncol = ngroup * 2 + 1, nrow = length(levels(x)) + 1)
      result[1, seq(2, ncol(result), by = 2)] <- apply(n, 2, sum)
      result[2:nrow(result), seq(2, ncol(result), by = 2)] <- n
      result[2:nrow(result), 1] <- paste0("- ", levels(x), " (n = ", apply(n, 1, sum), ")")

      # Check if summarystat.nice has more than one value per row and flatten it if necessary
      if (length(summarystat.nice) > 1) {
        result[2:nrow(result), seq(3, ncol(result), by = 2)] <- unlist(lapply(summarystat.nice, function(x) paste(x, collapse = ", ")))
      } else {
        result[2:nrow(result), seq(3, ncol(result), by = 2)] <- unlist(summarystat.nice)
      }
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


    # if (test == TRUE & ngroup > 1) {
    #   # overall Kruskal-Wallis test for group differences
    #   if (is.null(z)) {
    #     m <- 1:(length(x) * (1 - 0.5 * as.numeric(pooledGroup)))
    #     pval <- tryCatch(format.pval(kruskal.test(x = x[m], g = y[m])$p.value,
    #                                  eps = pcutoff, digits = pdigits, scientific = FALSE),
    #                      error = function(c) NA)
    #     result <- cbind(result, pval)
    #   } else {
    #     pval <- sapply(1:length(levels(x)), function(i) {
    #       q <- which(x == levels(x)[i])
    #       if (length(q) == 0) {
    #         out <- NA
    #       } else {
    #         m <- q[q %in% 1:(length(z) * (1 - 0.5 * as.numeric(pooledGroup)))]
    #         out <- tryCatch(format.pval(kruskal.test(x = z[m], g = y[m])$p.value,
    #                                     eps = pcutoff, digits = pdigits, scientific = FALSE),
    #                         error = function(c) NA)
    #       }
    #       return(out)
    #     })
    #     result <- cbind(result, c("", pval))
    #   }
    #
    # }
    return(result)
  }

  mycat.summary <- function(x, y, z, fullfreq = fullfreq) {
    ngroup <- length(levels(y))
    result <- matrix("", ncol = ngroup * 2 + 1, nrow = length(levels(x)) + 1)

    if (is.null(z)) {
      ta <- table(x, y)
      n <- if (bycol) {
        apply(ta, 2, sum)
      } else {
        if (pooledGroup) {apply(ta[, -ncol(ta)], 1, sum)} else {apply(ta, 1, sum)}
      }
      ta.prop <- if (bycol) {
        unclass(ta/rep(n, each = length(levels(x))))
      } else {
        unclass(ta/rep(n, ngroup))
      }

      ta.nice <- matrix(paste0(" (", formatC(100 * unclass(ta.prop), digits, format = "f"), "%)"),
                        nrow = nrow(ta), ncol = ncol(ta))
      result[2:nrow(result), 1] <- paste0("- ", levels(x))
      if (bycol) {
        #browser()
        result[1, seq(2, ncol(result), by = 2)] <- n
        if (fullfreq) {
          result[2:nrow(result), seq(3, ncol(result), by = 2)] <- paste0(ta, "/", rep(apply(ta, 2, sum), each = length(levels(x))), ta.nice)
        } else {
          result[2:nrow(result), seq(3, ncol(result), by = 2)] <- paste0(ta, ta.nice)
        }

      } else {
        result[2:nrow(result), 1] <- paste0(result[2:nrow(result), 1], " (n=", n, ")")
        if (fullfreq) {
          result[2:nrow(result), seq(3, ncol(result), by = 2)] <- paste0(ta, "/", rep(n, ngroup), ta.nice)
        } else {
          result[2:nrow(result), seq(3, ncol(result), by = 2)] <- paste0(ta, ta.nice)
        }

      }
    } else {
      tn <- table(x, y)
      tn2 <- if (pooledGroup) {tn[, -ncol(tn)]} else {tn}
      ta.nice <- matrix(by(z, list(x, y), function(z) {
        ta <- sum(z == TRUE, na.rm = TRUE)
        n <- length(na.omit(z))
        if (fullfreq) {
          output <- paste0(ta, "/", n, " (", formatC(100 * (ta/n), digits, format = "f"), "%)")
        } else {
          output <- paste0(ta, " (", formatC(100 * (ta/n), digits, format = "f"), "%)")
        }
        return(output)
      }), ncol = ngroup)
      result[2:nrow(result), 1] <- paste0("- ", levels(x), " (n=", apply(tn2, 1, sum), ")")
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
      mycat.summary(x = x, y = y, z = NULL, fullfreq = fullfreq)
    }
  } else {
    out <- if (is.factor(z)) {
      mycat.summary(x = x, y = y, z = z, fullfreq = fullfreq)
    } else {
      mycont.summary(x = x, y = y, z = z)
    }
  }



  out[1, 1] <- label_list
  return(out)
}

# create adverse event table ---------------------------------------------------

#' Create an adverse event summary table
#'
#' @description A function to create a simple adverse event summary table.
#'
#' @param ae_data a data frame contains adverse event data.
#' @param fullid_data a data frame contains treatment arm data of all participants (not just those had adverse event).
#' @param group_data a reference data frame contains the group name of each ae.
#' @param id.var a character specifies name of study id variable (exists in both adverse event data and treatment arm data).
#' @param aetype.var a character specifies name of adverse event type variable (exists in adverse event data).
#' @param grade.var NULL or character specifies name of adverse event grade variable (exists in adverse event data).
#' @param group.var a character specifies group (exists in adverse event data if group_data = NULL or group_data otherwise).
#' @param arm.var a character specifies name of treatment arm variable (exists in treatment arm data).
#' @param sort.by
#' An unquoted formula of sorting options.
#'
#' Available options are `ep`, `pt`, `p`, or a combination of them (eg ep+pt-p);
#' A minus sign indicates a descending order.
#' @param digits a number specifies number of significant digits for numeric statistics.
#' @param test a logical value specifies whether a statistical test will be performed to compare between treatment arms.
#' @param pdigits a number specifies number of significant digits for p value.
#' @param pcutoff a number specifies threshold value of p value to be displayed as "< pcutoff".
#' @param chisq.test
#' a logical value specifies whether Chi-squared test or Fisher's exact test will be used to compare between treatment arms.
#'
#' Be aware that even when chisq.test==TRUE, if expected values are < 1, the later test will take over.
#' @param correct a parameter for chisq.test().
#' @param simulate.p.value a parameter for chisq.test() and fisher.test().
#' @param B a parameter for chisq.test() and fisher.test().
#' @param workspace a parameter for fisher.test().
#' @param hybrid a parameter for fisher.test().
#' @param footer a vector of strings to be used as footnote of table.
#' @param flextable a logical value specifies whether output will be a flextable-type table.
#' @param bg a character specifies color of the odd rows in the body of flextable-type table.
#' @param group.var.priority a vector that specifies which groups will be appear first in the table.
#' @param print.aetype.header a logical value, whether to print the label of aetype.header.
#' @param na.text [`(Missing)`] in-placed text for missing AE
#'
#' @return a flextable-type table or a list with values/headers/footers
#' @import dplyr
#' @import tidyr
#' @export
sstable.ae <- function(ae_data, fullid_data, group_data = NULL, id.var,
                       aetype.var, grade.var = NULL,
                       group.var = NULL, group.var.priority = NULL, arm.var, sort.by, digits = 0,
                       test = TRUE, pdigits = 3, pcutoff = 0.001, chisq.test = FALSE, correct = FALSE,
                       simulate.p.value = FALSE, B = 2000, workspace = 1000000, hybrid = FALSE,
                       print.aetype.header =
                         length(aetype.var) > 1 | any(length(names(aetype.var)) > 0) | any(isTRUE(nchar(names(aetype.var)) > 0)),
                       na.text = '(Missing)',
                       footer = NULL, flextable = TRUE, bg = "#F2EFEE"){
  requireNamespace("dplyr")
  requireNamespace("tidyr")

  tmp <- match.call()
  print.aetype.header <- force(print.aetype.header)

  # if more than one aetype.var
  # perform sstable.ae for each for aetype.var
  # then do rbind
  if (length(aetype.var) > 1){

    # function to prepare
    make_tblcall <- function(orig_call, .aetype_var) {
      new_call <- orig_call
      new_call$aetype.var <- .aetype_var
      new_call$flextable <- FALSE
      new_call$print.aetype.header <- print.aetype.header
      new_call
    }
    env <- rlang::caller_env()
    n.grade <- if (length(grade.var)) length(unique(ae_data[[grade.var]])) else 0
    tbl1_call <- make_tblcall(tmp, aetype.var[1])
    tbl1 <- eval(tbl1_call, envir = env)
    tbl2p <-
      lapply(seq_along(aetype.var[-1]),
             function(i){
               .aetype.var <- aetype.var[i+1]
               tbl_call <- make_tblcall(tmp, .aetype.var)
               sstbl = eval(tbl_call, envir=env)
               sstbl
             })
    tbl2 <- tbl2p[[1]]

    # tbl2$table <- tbl2$table[-c(1:(3+n.grade)),]
    for (tbl3 in tbl2p[-1]) tbl2 <- ._do_rbind(tbl2, tbl3, header=c(1:(3+n.grade)))
    # browser()
    out <- ._do_rbind(tbl1, tbl2, header=c(1:(3+n.grade)))
    # rownames(out)[c(3,4,5,8)] <- 'section'
    if (flextable) return(ss_flextable(out))
    return(out)
  }

  ## check variable's name

  if (!id.var %in% names(ae_data)){stop(paste(tmp$id.var, "does not exist in", deparse(tmp$ae_data), "!!!"))}
  if (!id.var %in% names(fullid_data)){stop(paste(tmp$id.var, "does not exist in", deparse(tmp$fullid_data), "!!!"))}
  if (!aetype.var %in% names(ae_data)){stop(paste(tmp$aetype.var, "does not exist in", deparse(tmp$ae_data), "!!!"))}
  if (!is.null(grade.var)) {
    if (!grade.var %in% names(ae_data)){stop(paste(tmp$grade.var, "does not exist in", deparse(tmp$ae_data), "!!!"))}
  }
  if (!is.null(group.var)){
    if (!is.null(group_data)){
      if (!group.var %in% names(group_data)) stop(paste(tmp$group.var, 'does not exist in', deparse(tmp$group_data), '!!!'))
      if (!aetype.var %in% names(group_data)){stop(paste(tmp$aetype.var, "does not exist in", deparse(tmp$group_data), "!!!"))}
    } else
      if (!group.var %in% names(ae_data)) stop(paste(tmp$group.var, 'does not exist in', deparse(tmp$ae_data), '!!!'))
  }
  if (!arm.var %in% names(fullid_data)){stop(paste(tmp$arm.var, "does not exist in", deparse(tmp$fullid_data), "!!!"))}
  if (!missing(sort.by)){
    sort.by <- rlang::enquo(sort.by)
    sort.vars <- all.vars(sort.by)
    sort.signs <- getsign(sort.by)
    if (length(sort.signs) < length(sort.vars)) sort.signs <- purrr::prepend(sort.signs, quote(`+`))
    if (length(setdiff(sort.vars, c('pt','ep','p'))))
      stop('Sorting can only apply to `pt`, `ep`, and `p`')
  }

   #strip tibble class as it might cause problem
 	ae_data <- as.data.frame(ae_data)
  fullid_data <- as.data.frame(fullid_data)
  group_data <- as.data.frame(group_data)
  is.grouped <- !missing(group.var)

  # Check if grade.var is not NULL and has any NA values - hungtt
  if (!is.null(grade.var) && any(is.na(ae_data[[grade.var]]))) {
    # Replace NA values with "Grade NA"
    ae_data[[grade.var]][is.na(ae_data[[grade.var]])] <- "Grade N/A"
  }

  # If there is a designated name to aetype.var, populate that into the data
  if (!is.null(names(aetype.var)) & isTRUE(nchar(names(aetype.var))>0)){
    attr(ae_data[, aetype.var[[1]]], 'label') <- names(aetype.var)[[1]]
  }
  aetype.var <- aetype.var[[1]]

  # Check if any aetype.var is NA and replace with "NA"
  lbl = attr(ae_data[[aetype.var]], 'label')
  ae_data[[aetype.var]] = as.character(ae_data[[aetype.var]])
  ae_data[[aetype.var]] = structure(ae_data[[aetype.var]], label=lbl)
  if (any(is.na(ae_data[[aetype.var]]))) {
    #if (is.factor(ae_data[[var]]))
    #levels(ae_data[[var]]) <- c(levels(ae_data[[var]]), na.text )

    ae_data[[aetype.var]][is.na(ae_data[[aetype.var]])] <- na.text
  }

  ## format study arms
  idarm <- fullid_data[, c(id.var, arm.var)]; colnames(idarm) <- c("id", "arm")
  if (is.factor(idarm$arm)){
    ### [trinhdhk] 2024-04: reverse level of y for better summary
    idarm$arm <- addNA(idarm$arm, ifany = TRUE)
    idarm$arm <- ._lv_rev_(idarm$arm)
    arm_lev <- levels(idarm$arm)
  } else {
    ### [trinhdhk] 2024-04: reverse level of y for better summary
    arm_lev <- sort(unique(as.character(idarm$arm)), decreasing = TRUE, na.last = TRUE)
    idarm$arm <- with(idarm, factor(as.character(arm), levels = arm_lev, exclude = NULL))
  }

  # Original data frame ae_data assumed to exist
  # Ensure ae_data, ae_any, ae_grade, etc., are defined appropriately in your script

  replace_with_var_names <- function(df, var, aetype_var) {
  # Identify rows where aetype_var is not "NA"
    not_na_rows <- df[[aetype_var]] != na.text

    # Replace values with variable names or labels only where not "NA"
    if (!is.null(attr(df[[var]], "label"))) {
      df[[aetype_var]][not_na_rows] <- attr(df[[var]], "label")  # Use label if available
    } else {
      df[[aetype_var]][not_na_rows] <- var  # Fallback to default label
    }

    # Delete rows where aetype_var is "NA"
    df <- df[not_na_rows, , drop = FALSE]

    return(df)
  }

  # browser()

  # Example usage with ae_any data frame mutation
  ae_any <- ae_data  # Assuming ae_data is your original data frame

  mutated_data <- if(print.aetype.header)
    replace_with_var_names(ae_any, aetype.var, aetype.var) else NULL
  ae_any[, aetype.var] <- "Any selected adverse event"
  # Combine original and mutated data (assuming ae_data and ae_any exist)
  ae <- if(print.aetype.header)
		rbind(ae_data, ae_any, mutated_data) else
		rbind(ae_data, ae_any)

  # Extract grades of ae (assuming grade.var exists)
  if (!is.null(grade.var)) {
    grade <- sort(unique(na.omit(ae_data[, grade.var])))
    grade2 <- ifelse(grepl(pattern = "grade", ignore.case = TRUE, x = grade), grade, paste("Grade", grade))
    ae_grade <- do.call(rbind,
                        lapply(1:length(grade), function(i) {
                          tmpdat <- ae_data[ae_data[, grade.var] == grade[i], ]
                          tmpdat[, aetype.var] <- paste("-", grade2[i])
                          return(tmpdat)
                        }))
    ae <- rbind(ae, ae_grade)
  }

  # Select necessary columns and rename them
  ae <- ae[, c(id.var, aetype.var)]
  colnames(ae) <- c("id", "aetype")


  # Get unique levels of aetype
  aetype_lev.raw <- unique(as.character(ae_data[[aetype.var]]))

  # Check if aetype.var has a label attribute
  if (!is.null(attr(ae_data[[aetype.var]], "label"))) {
    aetype.var.label <- attr(ae_data[[aetype.var]], "label")
  } else {
    aetype.var.label <- aetype.var
 # Use default label
  }

  # Construct aetype_lev with labels where available
  aetype_lev <- c("Any selected adverse event",
                  if (!is.null(grade.var)) paste("-", grade2),
                  if (print.aetype.header) aetype.var.label,
                  aetype_lev.raw)

  ## add randomized arm to AE
  ae_arm <- merge(idarm, ae, by = "id", all.y = TRUE) %>%
    mutate(arm = addNA(factor(as.character(arm), levels = arm_lev, exclude = NULL), ifany = TRUE),
           aetype = addNA(factor(as.character(aetype), levels = aetype_lev, exclude = NULL), ifany = TRUE))
  idarm2 <- ae_arm |> select(id, arm)

  ## calculate episodes and patients
  ae_count <- ae_arm %>%
    group_by(aetype, arm) %>%
    summarise(n_episode = n(),
              n_patient = length(unique(id))) %>%
    ungroup() %>%
    complete(aetype, arm)
  if (!is.factor(ae_count$aetype)) {ae_count$aetype <- factor(ae_count$aetype, levels = levels(ae_arm$aetype))}
  if (!is.factor(ae_count$arm)) {ae_count$arm <- factor(ae_count$arm, levels = levels(ae_arm$arm))}

  episode_n <- unlist(spread(select(as.data.frame(ae_count), -n_patient), key = arm, value = n_episode)[, -1])
  episode_n[is.na(episode_n)] <- 0
  patient_n <- unlist(spread(select(ae_count, -n_episode), key = arm, value = n_patient)[, -1])
  patient_n[is.na(patient_n)] <- 0
  patient_N <- rep(table(idarm$arm), each = nlevels(ae_arm$aetype))
  # tryCatch(patient_n/patient_N, warning=function(w) browser())
  patient_p <- patient_n/patient_N

  ## table
  #browser()
  value <- matrix(ncol = nlevels(ae_arm$arm)*3, nrow = nlevels(ae_arm$aetype))
  value[, seq(from = 1, to = nlevels(ae_arm$arm)*2, by = 2)] <- episode_n
  value[, seq(from = 2, to = nlevels(ae_arm$arm)*2, by = 2)] <- paste0(patient_n, "/", patient_N,
                                                                       " (", formatC(100 * patient_p, digits, format = "f"), "%)")
  ## add raw values to 2 dummy columns for sorting purpose
  value[, (nlevels(ae_arm$arm)*2 + 1):(nlevels(ae_arm$arm)*3)] <- patient_n
  ae_value <- cbind(aename = levels(ae_arm$aetype), value)

  ## get the aehead -trinhdhk
  ae_value.head <- as.data.frame(ae_value, stringsAsFactors = FALSE) %>% filter(!aename %in% aetype_lev.raw)

  ## categorizing ae into groups
  ## author: trinhdhk
  if (!is.null(group.var)) {
    if (is.null(group_data)) {
      group_data <- unique(ae_data[,c(aetype.var, group.var)])
    }
    group_data[[group.var]] <- replace_na(as.character(group_data[[group.var]]), 'Uncategorised')
    if (!is.factor(group_data[, group.var])) group_data[, group.var] <- as.factor(group_data[, group.var])
    group_data <- group_data[, c(aetype.var, group.var)]
    group_data[,aetype.var] <- as.character(group_data[,aetype.var])
    names(group_data) <- c(aetype.var, '.tmp.group')

    if (!is.null(group.var.priority)) {
      # Reorder group.var based on group.var.priority
      group_data <- group_data %>%
        mutate(.tmp.group = factor(.tmp.group, levels = c(group.var.priority, setdiff(unique(.tmp.group), group.var.priority)), ordered = TRUE))
    }
    # } else {
    #   ## - create a fake grouping
    #   group_data <- data.frame(ae = aetype_lev.raw, group=1, stringsAsFactors = FALSE)
    #   names(group_data) <- c(aetype.var, '.tmp.group')
    # }

    # browser()

    ### bind group back to summary table -trinhdhk
    ae_value.headless <-
      as.data.frame(ae_value, stringsAsFactors = FALSE) %>%
      filter(aename %in% aetype_lev.raw) %>%
      rename({{aetype.var}} := aename) %>%
      right_join(group_data, by = aetype.var) %>%
      group_by(.tmp.group) %>%
      group_modify(~ rbind(c(as.character(.y$.tmp.group),
                             unlist(lapply(seq_along(arm_lev),
                                           function(i){
                                             return(c(
                                               sum(as.numeric(unlist(.x[, 2*i]))),
                                               ''))
                                             # paste(sum(as.numeric(unlist(.x[, length(arm_lev)*2+i+1])))))
                                           })),
                             rep('',ncol(ae_value)-1-2*length(arm_lev))
      ),.x))
    # browser()

    grouptitle_index <- ae_value.headless %>% group_rows() %>% sapply(., function(x) head(x, 1))

    ae_value.headless <-
      ae_value.headless %>%
      ungroup %>%
      select(-.tmp.group)

    #adjust the row number of group title by combining with the head
    grouptitle_index <- grouptitle_index + nrow(ae_value.head)
    ae_value <-
      ae_value.head %>%
      `names<-`(names(ae_value.headless)) %>%
      rbind(ae_value.headless)
  } else ae_value <- as.data.frame(ae_value, stringsAsFactors = FALSE) %>% rename({{aetype.var}} := aename)
  # browser()

  ## test
  if (test) {
    ## <trinhdhk:
    ## - requested by Ronald, add the ability to override the chisq.test with fisher.test when the expected var is <1.
    ## - this should work even when chisq.test == T
    ## >

    do_fisher <- function(mat, workspace = workspace, hybrid = hybrid,
                          simulate.p.value = simulate.p.value, B = B){
      tryCatch(format.pval(fisher.test(x = mat, workspace = workspace, hybrid = hybrid,
                                       simulate.p.value = simulate.p.value, B = B)$p.value,
                           eps = pcutoff, digits = pdigits, scientific = FALSE),
               error = function(e) NA)
    }

    do_chisq <- function(mat, correct = correct,
                         simulate.p.value = simulate.p.value, B = B,
                         workspace = workspace, hybrid = hybrid){
      chi.out <- tryCatch(suppressWarnings(chisq.test(x = mat, correct = correct,
                                                      simulate.p.value = simulate.p.value, B = B)),
                          error = function(e) NA)
      if (length(chi.out) == 1) # that implicits the NA value, so no more NA check here
        return(chi.out)

      if (any(chi.out$expected<1))
        return(do_fisher(mat=mat, workspace = workspace, hybrid = hybrid,
                         simulate.p.value = simulate.p.value, B = B))
      #else
      return(format.pval(chi.out$p.value,eps = pcutoff, digits = pdigits, scientific = FALSE))
    }

    pval <- sapply(1:nrow(value), function(i) {
      idx <- seq(from = i, to = length(patient_n), by = nrow(value))
      mat <- matrix(c(patient_n[idx], patient_N[idx] - patient_n[idx]), nrow = 2, byrow = TRUE)
      out <- if (chisq.test) {
        # tryCatch(format.pval(chisq.test(x = mat, correct = correct,
        #                                 simulate.p.value = simulate.p.value, B = B)$p.value,
        #                      eps = pcutoff, digits = pdigits, scientific = FALSE),
        #          error = function(c) NA)
        do_chisq(mat, correct = correct,
                 simulate.p.value = simulate.p.value, B = B,
                 workspace = workspace, hybrid = hybrid)
      } else {
        # tryCatch(format.pval(fisher.test(x = mat, workspace = workspace, hybrid = hybrid,
        #                                  simulate.p.value = simulate.p.value, B = B)$p.value,
        #                      eps = pcutoff, digits = pdigits, scientific = FALSE),
        #          error = function(c) NA)
        do_fisher(mat, workspace = workspace, hybrid = hybrid,
                  simulate.p.value = simulate.p.value, B = B)
      }
      return(out)
    })
    pval[is.na(pval)] <- "-"

    ## add grouping row - trinhdhk
    # browser()
    ae_value <-
      left_join(
        ae_value,
        cbind(aetype=aetype_lev, pval) %>%
          as.data.frame(stringsAsFactors = FALSE) %>%
          rename({{aetype.var}} := aetype),
        by = aetype.var
      ) %>%
      mutate(pval = replace_na(as.character(pval), ''))
  }

  # browser()
  ## sorting - trinhdhk
  if (!missing(sort.by)){
    ## - the head will not be sorted.
    split.parts <- c(1, if(is.null(group.var)) (nrow(ae_value.head)+1) else grouptitle_index)
    col.name <- arrange(mutate(expand.grid(c('ep', 'n.pt'), 1:nlevels(ae_arm$arm)), out = paste(Var1, Var2, sep='.')), Var2)$out
    dummy.col.name <- paste('pt', 1:nlevels(ae_arm$arm), sep='.')
    colnames(ae_value) <- c('aetype', col.name, dummy.col.name, if (test) 'p')
    ae_value <-
      mutate_at(as.data.frame(ae_value, stringsAsFactors = FALSE),
                vars(matches('(^pt.\\d)|(^ep.\\d)'), dummy.col.name), as.numeric) %>%
      mutate(
        pt.all = by(select(., starts_with('pt.')), 1:nrow(.), sum),
        ep.all = by(select(., starts_with('ep.')), 1:nrow(.), sum)
      )

    ae_value.group <- lapply(seq_along(split.parts),
                             function(i){
                               return(ae_value[split.parts[i]:(c(split.parts, nrow(ae_value) + 1)[i+1]-1),])
                             })
    ## - remove the head
    ae_value.head <- ae_value.group[[1]]
    ae_value.group.headless <- ae_value.group[-1]

    ## - convert the sign and var into arrange-friendly quosures.

    arrange.params <- rlang::as_quosures(
      unlist(lapply(seq_along(sort.vars),
                    function(i){
                      sort.var <- sort.vars[[i]]
                      sort.sign <- sort.signs[[i]]
                      if (identical(sort.sign, quote(`+`)))
                        lapply(paste0('~', sort.var, if (sort.var != 'p') paste0('.', c('all',seq_along(arm_lev)))), env = parent.frame(), as.formula)
                      else
                        lapply(paste0('~desc(', sort.var,if (sort.var != 'p') paste0('.', c('all',seq_along(arm_lev))), ')'), env = parent.frame(), as.formula)
                    })
      ), env=NULL)


    ae_value.sorted <- do.call(rbind,
                               lapply(ae_value.group.headless,
                                      function(grouped_ae){
                                        if (is.grouped){
                                          rbind(
                                            grouped_ae[1, ],
                                            do.call(arrange, rlang::flatten(list(
                                              .data=as.data.frame(grouped_ae, stringsAsFactors = FALSE)[-1, ],
                                              unlist(arrange.params))))
                                          )
                                        } else {
                                          do.call(arrange, rlang::flatten(list(
                                            .data=as.data.frame(grouped_ae, stringsAsFactors = FALSE),
                                            unlist(arrange.params))))
                                        }
                                      }))
    ## - bind the head again
    ae_value <- rbind(ae_value.head, ae_value.sorted) %>% mutate_all(as.character) %>% mutate_all(replace_na, '') %>% select(-pt.all, -ep.all)

    # browser()
  }

  ## remove the dummy col -trinhdhk
  ae_value <- ae_value[,-(nlevels(ae_arm$arm)*2 + 2):-(nlevels(ae_arm$arm)*3+1)]
  ae_value <- as.matrix(ae_value) #convert back to matrix to avoid conflicts
  colnames(ae_value) <- NULL
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
  footer <- c("n episode refers to the number of adverse events in each study arm.",
              "n patient refers to the number of patients with at least one event in each study arm.",
              if (any(value == "-")) "- : value cannot be estimated." else NULL,
              if (test) {paste("p-values were based on",
                               ifelse(chisq.test == FALSE, "Fisher's exact test", "Chi-squared test if applicable and Fisher's exact test otherwise "),
                               "comparing n patient between study arms for each type of adverse event.")} else NULL,
              footer)

  ### flextable
  if (flextable) {
    requireNamespace("flextable")
    requireNamespace("officer")
    # return(ss_flextable({
    #   tab <- list(table = rbind(header1, header2, ae_value),
    #               footer = footer)
    #   class(tab$table) <- c('ae_tbl', 'ss_tbl', class(tab$table))
    #   class(tab) <- c('ss_ae','ss_obj')
    #   tab
    # }))

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
    for (k in (1:length(footer))) {
      tab <- flextable::add_footer(tab, V1 = footer[k], top = FALSE)
      tab <- flextable::merge_at(tab, i = k, j = 1:length(header1), part = "footer")
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
    tab <- flextable::border_remove(tab)
    tab <- flextable::hline(tab, border = tabbd, part = "header")
    tab <- flextable::hline_top(tab, border = tabbd, part = "all")
    tab <- flextable::hline_bottom(tab, border = tabbd, part = "body")
    if (print.aetype.header){
      tab <- flextable::bold(
        tab,
        i = 2 + if (length(grade.var)) length(unique(ae_data[[grade.var]])) else 0,
        j=1, part = "body")
    }

    ### group-name rows-trinhdhk
    if (is.grouped) {
      tab <- flextable::merge_h_range(tab, grouptitle_index, 1, ncol(ae_value))
      tab <- flextable::bold(tab, i = grouptitle_index, part = "body") # Bold the group.var rows - hungtt
    }

  } else {
    tab <- list(table = rbind(header1, header2, ae_value),
                footer = footer)
    if (print.aetype.header){
      rownames(tab$table)[[4 + if (length(grade.var)) length(unique(ae_data[[grade.var]])) else 0]] <- 'section'
    }
    class(tab$table) <- c('ae_tbl', 'ss_tbl', class(tab$table))
  }
  if (!flextable) class(tab) <- c('ss_ae','ss_obj')
  return(tab)
}

# create survival comparison table ----------------------------------------

#' Summarize results for a Cox survival model or restricted mean survival time with the treatment arm as the main covariate
#'
#' @description A function to summarize results for a survival model with the treatment arm (variable "arm") as the main covariate
#'
#' @param model a formula which can be used to fit the survival model. This formula can include other covariates than arm BUT arm must be the first covariate in the model.
#' @param data a data frame to fit the survival model.
#' @param add.risk [\code{TRUE}] a logical value specifies whether the event probability ("absolute risk")
#' @param time [\code{Inf}] the truncation time, affecting the descriptive and the RMST model, set to \code{Inf} to perform analyses at maximum time available
#' (minimax of the observed time across two arms in RMST model)
#' @param reference.arm [\code{B}] reference arm, default to the second arm ("B"), change to "A" for base on the first arm
#' @param compare.method [\code{cox}] a string, either "cox" for CoxPH model, "cuminc" for cumulative incidence, or "rmst" for restricted mean survival time.
#' Note that if "cox" is specified and model is a mstate model, a Fine-Gray model is used.
#' If CoxPH is preferred, used Surv(t, ev == 'event-of-interest') on the LHS.
#'
#' @param compare.args: a list of additional args for compare.methods,
#'
#'  For compare.method = 'cox', it is
#'  `add.prop.haz.test` [\code{TRUE}]: a logical value specifies whether a test for proportional hazards should be added,, additional args are fed directly to `survival::coxph`.
#'  only when model is a mstate model, `cause`, default to whatever the first cause.
#'
#'  For compare.method = 'cuminc', args are fed to \code{\link[eventglm:cumincglm]{cumincglm}}
#'  `type`: [\code{diff}] a string, "diff" for difference in cumulative incidence, "ratio' for ratio of cumulative incidence,
#'  other optional args include: model.censoring, formula.censoring, ipcw.method. See \code{\link[eventglm:cumincglm]{cumincglm}} for more details.
#'
#'  For compare.method = 'rmst', args are fed to \code{\link[eventglm:rmeanglm]{rmeanglm}} ,
#'  `type`: [\code{diff}] a string, "diff" for difference in RMST, "ratio' for ratio of RMST, "lost.diff" for difference in restricted mean time lost (RMTL, = -diff), and "lost.ratio" for ratio of RMTL
#'  other optional args include: model.censoring, formula.censoring, ipcw.method. See \code{\link[eventglm:rmeanglm]{rmeanglm}} for more details.
#' @param add.prop.haz.test [\code{TRUE}] (legacy, depricated), please move this to compare.args
#' @param medsum [\code{TRUE}] a logical value, specifying whether median (IQR) of time to event should be described.
#' @param p.compare [\code{TRUE}] a logical value, specifying whether we should report p-value for the main comparison
#' @param digits [\code{2}] a number specifies number of significant digits for numeric statistics.
#' @param pdigits [\code{3}] a number specifies number of significant digits for p value.
#' @param pcutoff [\code{0.001}] a number specifies threshold value of p value to be displayed as "< pcutoff".
#' @param footer a [\code{NULL}] vector of strings to be used as footnote of table.
#' @param flextable [\code{TRUE}] a logical value specifies whether output will be a flextable-type table.
#' @param bg [\code{#F2EFEE}] a character specifies color of the odd rows in the body of flextable-type table.
#' @return a flextable-type table or a list with values/headers/footers
#'
#' @author This function was originally written by Marcel Wolbers. Lam Phung Khanh did some modification.
#' @import survival
#' @export
sstable.survcomp <- function(
    model, data, add.risk = TRUE,
    time = Inf,
    reference.arm = c('B', 'A'),
    compare.method = c('cox', 'rmst', 'cuminc'),
    compare.args = list(),
    add.prop.haz.test = TRUE, medsum = TRUE,
    p.compare = TRUE,
    digits = 2, pdigits = 3, pcutoff = 0.001, footer = NULL, flextable = TRUE, bg = "#F2EFEE"){
  requireNamespace("survival")

  # Initialise the table --------------------------
  ## strip the tibble class which causes issue - trinhdhk
  data <- as.data.frame(data)

  mf <- model.frame(update(model, .~1), data=data)
  NAs <- attr(mf, 'na.action') |> unname()

  if (length(NAs)){
    warning(sprintf('Missing values on observation(s) %s',
                    paste(NAs,collapse=', ')))
    # data <- data[seq_len(nrow(data))[-NAs],]
    data <- dplyr::slice(data, seq_len(nrow(data))[-NAs])
  }

  compare.method <- match.arg(compare.method)
  reference.arm <- match.arg(reference.arm)

  # arm.var <- if (length(model[[3]]) > 1) {deparse(model[[3]][[2]])} else {deparse(model[[3]])}
  arm.var <- formula.tools::rhs.vars(model)[[1]]
  if (!inherits(data[, arm.var], "factor")) data[, arm.var] <- factor(data[, arm.var])
  ### [trinhdhk] 2024-04: reverse level of y for better summary
  data[, arm.var] <- ._lv_rev_(data[, arm.var])
  arm.names <- levels(data[, arm.var])
  if (length(arm.names) > 2) stop('At the moment, only two-arm studies are supported. A lot of code has covered more than 2 arms but please think about what should be compared in those cases (chunk test or pairwise test for instance) before implementing / trinhdhk.')


  # Prepare survfit
  time2 <- if (time == Inf) .Machine$integer.max else time
  fit.surv0 <- survival::survfit(update(model, new = as.formula(paste0(". ~ ", arm.var))), data = data)
  # [Trinhdhk] Use integer.max instead of Inf b/c summary.survfit does not want Inf anymore. 05/24

  # Check for competing risk aka mstate -trinhdhk
  ms <- inherits(fit.surv0, 'survfitms')
  if (ms) {
    all_causes <- attr(mf[[1]], 'states')
    if(length(unique(mf[[1]][,2]))<2)
      stop('You wanted to use a mstate model. But only one event happened.')

    if (is.null(compare.args$cause)) compare.args$cause <- all_causes[[1]]
    if (!compare.args$cause %in% all_causes)
      stop('Specified cause is not available in the list of causes.
If you are running this in survcomp.subgroup, perhaps in one subgroup an event did not happen. I don\'t know how to fix this.')
    this_cause <- which(all_causes == compare.args$cause)+1
  }


  # Table header
  header1 <- c(paste(arm.names, " (n=", table(data[, arm.var]), ")", sep = ""), "Comparison")
  compare.stat <- switch(compare.method,
                         cox = 'HR',
                         cuminc = if (is.null(compare.args$type)) 'Cumul.inc difference'
                         else switch(compare.args$type,
                                     diff = 'Cumul.inc difference',
                                     ratio = 'Cumul.inc ratio',
                                     stop('Illegal type for cumulative incidence comparison model')),
                         rmst = if (is.null(compare.args$type)) 'RMST difference'
                         else switch(compare.args$type,
                                     diff = 'RMST difference',
                                     lost.diff = 'RMTL difference',
                                     ratio = 'RMST ratio',
                                     lost.ratio = 'RMTL ratio',
                                     stop('Illegal type for RMST comparison model')))

  header2 <- c(rep(ifelse(add.risk, "events/n (risk [%])", "events/n"), length(arm.names)), paste(compare.stat, if (p.compare) "(95%CI); p-value" else "(95%CI)"))
  header <- rbind(header1, header2)
  result <- rbind(header, "")

  ## footer
  compare.note <- switch(
    compare.stat,
    'HR' = 'HR = hazard ratio',
    'Cumul.inc difference' = 'Cumul.inc : Cumulative incidence',
    'Cumul.inc ratio' = 'Cumul.inc : Cumulative incidence',
    'RMST difference' = 'RMST: Restricted mean survival time',
    'RMTL difference' = 'RMTL: Restricted mean time loss',
    'RMST ratio' = 'RMST: Restricted mean survival time',
    'RMTL ratio' = 'RMTL: Restricted mean time lost'
  )
  compare.name <- switch(compare.method,
                         cuminc='Generalized linear models for cumulative incidence',
                         cox=if (ms) 'Fine-Gray model' else 'Cox proportional hazards model',
                         rmst='Restricted mean survival time model')
  footer <- c(
    paste0(compare.note, "; IQR = interquartile range."),
    paste(compare.stat, if (p.compare) "and p value", "were based on",
          paste0(compare.name, '.')),
    footer)


  # Descriptive analysis ---------------------------
  # add number of events and risks

  fit.surv <- summary(fit.surv0, time = time2, extend = TRUE)

  # [Trinhdhk] This is crap as always returns at inf
  # if (length(unique(data[, arm.var])) < length(arm.names)) {
  #   tmp <- fit.surv$table
  #   dim(tmp) <- c(length(unique(data[, arm.var])), length(tmp))
  #   colnames(tmp) <- names(fit.surv$table)
  # } else {
  #   tmp <- fit.surv$table
  # }
  # browser()
  n.event <- if (ms) fit.surv$n.event[,this_cause] else fit.surv$n.event
  events.n <- paste(n.event, fit.surv$n, sep = "/")
  if (add.risk) {
    # cumhaz <- if (ms) fit.surv$cumhaz[, this_cause-1] else fit.surv$cumhaz
    # risk <- if (ms)
    #   n.event / with(fit.surv, n.event[,this_cause]+n.risk[,this_cause]+n.censor[,this_cause]) else
    #   n.event / with(fit.surv, n.event+n.risk+n.censor)
    risk <- n.event / fit.surv$n
    events.n <- paste(events.n,
                      " (", formatC(100*(risk), digits, format = "f"), ")", sep="")
  }
  idx <- which(arm.names %in% unique(data[, arm.var]))
  result[3, 1:length(arm.names)] <- rep("-", length(arm.names))
  result[3, idx] <- events.n



  # Comparison --------------------------------------
  # Re-base the arm factor
  if (reference.arm == 'B') data[, arm.var] <- ._lv_rev_(data[, arm.var])
  if (compare.method == "cox"){
    if (!is.null(compare.args$add.prop.haz.test))
      add.prop.haz.test <- compare.args$add.prop.haz.test
    if (length(events.n) < length(arm.names)) {
      result[3, length(arm.names) + 1] <- "-"
      if (add.prop.haz.test){result <- cbind(result, c("Test for proportional hazards", "p-value", "-"))}
    } else {
      compare.args$add.prop.haz.test <- NULL
      compare.args$formula <- model
      compare.args$data <- data

      # add HR, CI, p-value
      fit.coxph <- if (!ms)
        do.call(survival::coxph, compare.args) #survival::coxph(model, data)
      else{
        compare.args$etype <- compare.args$cause
        compare.args$cause <- NULL
        fml <- force(update(model, Surv(fgstart,fgstop,fgstatus)~.))
        fg <- do.call(survival::finegray, compare.args)
        eval(substitute(survival::coxph(fml, data = fg,  weights=fgwt), list(fml=fml)))
      }
      est <- coef(fit.coxph)[1:(length(arm.names)-1)] #trinhdhk: if there were more covariables than just arm, subsetting this to only get the arm
      hr <- formatC(exp(est), digits, format = "f")

      result[3, length(arm.names) + 1] <-
        ifelse(is.na(est),"-",

        {
          # browser()
          se <- sqrt(fit.coxph$var[[1]]) # trinhdhk: if there were more covariables than just arm, $var returns a var-cov mat, this first element is the var of arm (given arm is the first covariable)
          z <- abs(est/se)
          pval <- format.pval((1 - pnorm(z)) * 2, eps = pcutoff, digits = pdigits)
          ci <- sapply(est, \(.est) {
            paste(formatC(exp(c(.est - qnorm(0.975) * se, .est + qnorm(0.975) * se)), digits, format = "f"), collapse = ", ")
          })
          if (p.compare){
            hr.ci.p <- paste(hr, " (", ci, "); p=", pval, sep = "")
          } else {
            hr.ci.p <- paste(hr, " (", ci, ")", sep = "")
          }

          hr.ci.p
        })


      # add test for proportional hazards
      if (add.prop.haz.test){
        if (is.na(est)) {
          result <- cbind(result, c("Test for proportional hazards", "p-value", "-"))
        } else {
          attr(model, ".Environment") <- environment() # needed for cox.zph to work
          p.prop.haz <-
            if(!ms){
              cox <- do.call(survival::coxph, compare.args)
              survival::cox.zph(cox)$table[1, "p"]
            } else{
              fml <- force(update(model, Surv(fgstart,fgstop,fgstatus)~.))
              fg <- do.call(survival::finegray, compare.args)
              cox <- eval(substitute(survival::coxph(fml, data = fg,  weights=fgwt), list(fml=fml)))
              survival::cox.zph(cox)$table[1, "p"]
            }
          if (is.na(p.prop.haz)) {
            p.prop.haz <- "-"
          } else {
            p.prop.haz <- format.pval(p.prop.haz, eps = pcutoff, digits = pdigits)
          }
          result <- cbind(result, c("Test for proportional hazards", "p-value", p.prop.haz))
        }
      }
    }
  } else {
    # requireNamespace('eventglm')
    if (length(events.n) < length(arm.names)) {
      result[3, length(arm.names) + 1] <- "-"
    } else {
      compare.args$time <- time
      compare.args$add.prop.haz.test <- NULL
      compare.args$formula <- model
      compare.args$data <- data
      type <- if (is.null(compare.args$type)) 'diff' else compare.args$type
      compare.args$type <- NULL
      compare.args$link <- switch(type,
                                  "lost.diff" = 'identity',
                                  "diff" = 'identity',
                                  "ratio" = 'log',
                                  'lost.ratio' = 'log')
      if (is.null(compare.args$model.censoring)) compare.args$model.censoring <- 'stratified'
      if (is.null(compare.args$formula.censoring)) compare.args$formula.censoring <- as.formula(paste0("~`", arm.var, '`'))
      # Extract the time from Surv object with the arm included
      mf <- model.frame(update(model, new = as.formula(paste0(". ~`", arm.var, '`'))), data = data)
      mf <- cbind(unclass(mf[,1]), mf[,2])
      # Get max of stop time, is the minimum of last observed time between two arms.
      # The stop time for right cens data is the first column ($time), for interval-cens data it is the second column ($stop)
      # With laziness it is coded as mf[, ncol(mf)-2] (the last two columns are the event status from Surv object, and the arm)
      # browser()
      minimax.time <- min(by(mf[, ncol(mf)-2], mf[, ncol(mf)], max))
      max.time <- max(mf[, ncol(mf)-2])
      # tau for rmst is capped as minamax.time
      if (is.null(compare.args$time)) compare.args$time <- minimax.time
      else if (compare.args$time > minimax.time) {
        if (time != Inf) warning(
          sprintf('Specified time (tau) is later than maximum observed time between two arms (%.2f). Set too %.2f', minimax.time, minimax.time)
        )
        compare.args$time <- minimax.time
      }
      # If type == lost.ratio then reverse the time
      # i.e. Surv(tau - ev_time, ev) ~ .
      if (type == 'lost.ratio' && !ms) {
        if (compare.method == 'cuminc') stop('type lost.ratio is meaningless in cuminc comparison.')
        model.lhs <- formula.tools::lhs(model)
        for (i in 2:(length(model.lhs)-1)) {
          old <- model.lhs[[i]];
          model.lhs[[i]] <- substitute(max.time - old, list(old=old, max.time=max.time))
        }
        formula.tools::lhs(model) <- model.lhs
        # browser()
        compare.args$formula <- model
      }

      if (type=='ratio' && ms) {
        stop('Don\'t know how to quantify RMST ratio for competing risks. See ?rmeanglm for more details.')
      }

      # Perform rmeanglm
      # rmeanglm <- eventglm::rmeanglm
      fitter <- if (compare.method == 'rmst') eventglm::rmeanglm else eventglm::cumincglm
      fit.rmst <- do.call(fitter, compare.args)
      est <- coef(fit.rmst)[2:(length(arm.names))] # get the coef for arm
      invlink <- fit.rmst$family$linkinv
      # i don't know why but rmeanglm decides to quantify RMST for non-ms and
      # RMTL for ms models.
      if ((type=='diff' && ms)|(type=='lost.diff'&&!ms)) invlink <- function(x) -x # loss.diff=-diff
      diff <- formatC(invlink(est), digits, format = "f")

      result[3, length(arm.names) + 1] <-
        ifelse(is.na(est),"-",
               {
                 summary.rmst <- summary(fit.rmst)$coefficients
                 p <- summary.rmst[2:length(arm.names), 'Pr(>|z|)']
                 pval <- format.pval(p, eps = pcutoff, digits = pdigits)
                 cf <- confint(fit.rmst)[2:length(arm.names),, drop=FALSE]
                 ci <- apply(cf, 1,
                             \(.cf) paste(formatC(sort(invlink(.cf)), digits, format = "f"), collapse = ", ")
                 )
                 if (is.na(p)) diff.ci.p <- paste(diff, '(-)')
                 else if (p.compare){
                   diff.ci.p <- paste(diff, " (", ci, "); p=", pval, sep = "")
                 } else {
                   diff.ci.p <- paste(diff, " (", ci, ")", sep = "")
                 }
                 diff.ci.p
               })
    }

  }

  rownames(result) <- NULL

  # add label
  #browser()
  varname <- all.vars(model)[2:1]
  varlabel <- sapply(varname, function(x){
    ifelse(is.null(attr(data[, x], "label")), x, attr(data[, x], "label"))
  })
  # output
  output <- if (medsum & !ms) {
    result <- rbind(result, "", "", "", "")
    # add median (IQR) of time-to-event
    qfit <- as.matrix(do.call(cbind, quantile(fit.surv0, probs = c(0.5, 0.25, 0.75))))
    result[5:7, 1:length(arm.names)] <- apply(formatC(qfit, digits, format = "f"), 1, function(x){
      sapply(1:3, function(z) paste(x[z], " (", x[z + 3], ", ", x[z + 6], ")", sep = ""))
    })
    cbind(c("Endpoint", "", varlabel, "- Median (95%CI)", "- Lower IQR (95%CI)", "- Upper IQR (95%CI)"), result)
  } else {
    output <- cbind(c("Endpoint", "", varlabel[1]), result)
  }
  rownames(output) <- NULL
  value <- output[-c(1:2), , drop = FALSE]
  #browser()
  ## flextable
  if (flextable) {
    requireNamespace("flextable")
    requireNamespace("officer")

    ## main table
    tab <- flextable::flextable(as.data.frame(value))

    ## header
    header1 <- output[1, ]; header2 <- output[2, ]
    header2[1] <- header1[1]
    assign("tab",
           eval(parse(text = paste0("flextable::set_header_labels(tab,",
                                    paste(paste0("V", 1:length(header1)), paste0("'", header1, "'"), sep = "=", collapse = ","),
                                    ")"))))
    assign("tab",
           eval(parse(text = paste0("flextable::add_header(tab,",
                                    paste(paste0("V", 1:length(header1)), paste0("'", header2, "'"), sep = "=", collapse = ","),
                                    ", top = FALSE)"))))

    tab <- flextable::merge_v(tab, part = "header")

    if (any(value == "-")) footer <- c(footer, "- : value cannot be estimated.")

    for (k in (1:length(footer))) {
      tab <- flextable::add_footer(tab, V1 = footer[k], top = FALSE)
      tab <- flextable::merge_at(tab, i = k, j = 1:length(header1), part = "footer")
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
    tab <- flextable::border_remove(tab)
    tab <- flextable::hline(tab, border = tabbd, part = "header")
    tab <- flextable::hline_top(tab, border = tabbd, part = "all")
    tab <- flextable::hline_bottom(tab, border = tabbd, part = "body")

  } else {
    tab <- list(table = output,
                footer = footer)
    class(tab$table) <- c('survcomp_tbl', 'ss_tbl', class(tab$table))
  }
  if (!flextable) class(tab) <- c('ss_survcomp','ss_obj')
  return(tab)
}

#' Summarize results for a survival model by treatment arm and subgroup
#'
#' @description A function to summarize results for a survival model by treatment arm (variable "arm") and subgroup.
#'
#' @param base.model a formula from which sub-group specific estimates are extracted (!! arm must be the first covariate in the model).
#' @param overall.model an optional one-sided formula for additional terms for overall population only
#' @param subgroup.model a formula of the form "~subgrouping.variable1+subgrouping.variable2" (!! subgrouping.variable must be factors and there should be nothing on the left-hand side of the formula).
#' @param data a data frame to fit the survival model.
#' @param add.risk [\code{TRUE}] a logical value specifies whether the event probability ("absolute risk") at time "infinity" should be displayed.
#' @param time [\code{Inf}] the truncation time, affecting the descriptive and the RMST model, set to \code{Inf} to perform analyses at maximum time available
#' (minimax of the observed time across two arms in RMST model)
#' @param reference.arm [\code{B}] reference arm, default to the second arm ("B"), change to "A" for base on the first arm
#' @param compare.method [\code{cox}] a string, either "cox" for CoxPH model, "cuminc" for cumulative incidence, or "rmst" for restricted mean survival time.
#' Note that if "cox" is specified and model is a mstate model, a Fine-Gray model is used.
#' If CoxPH is preferred, used Surv(t, ev == 'event-of-interest') on the LHS.
#'
#' @param compare.args: a list of additional args for compare.methods,
#'
#'  For compare.method = 'cox', it is
#'  `add.prop.haz.test` [\code{TRUE}]: a logical value specifies whether a test for proportional hazards should be added,, additional args are fed directly to `survival::coxph`.
#'  only when model is a mstate model, `cause`, default to whatever the first cause.
#'
#'  For compare.method = 'cuminc', args are fed to \code{\link[eventglm:cumincglm]{cumincglm}}
#'  `type`: [\code{diff}] a string, "diff" for difference in cumulative incidence, "ratio' for ratio of cumulative incidence,
#'  other optional args include: model.censoring, formula.censoring, ipcw.method. See \code{\link[eventglm:cumincglm]{cumincglm}} for more details.
#'
#'  For compare.method = 'rmst', args are fed to \code{\link[eventglm:rmeanglm]{rmeanglm}} ,
#'  `type`: [\code{diff}] a string, "diff" for difference in RMST, "ratio' for ratio of RMST, "lost.diff" for difference in restricted mean time lost (RMTL, = -diff), and "lost.ratio" for ratio of RMTL
#'  other optional args include: model.censoring, formula.censoring, ipcw.method. See \code{\link[eventglm:rmeanglm]{rmeanglm}} for more details.
#' @param p.compare [\code{TRUE}] a logical value, specifying whether we should report p-value for the main comparison
#' @param digits [\code{2}] a number specifies number of significant digits for numeric statistics.
#' @param pdigits [\code{3}] a number specifies number of significant digits for p value.
#' @param pcutoff [\code{0.001}] a number specifies threshold value of p value to be displayed as "< pcutoff".
#' @param footer a [\code{NULL}] vector of strings to be used as footnote of table.
#' @param flextable [\code{TRUE}] a logical value specifies whether output will be a flextable-type table.
#' @param bg [\code{#F2EFEE}] a character specifies color of the odd rows in the body of flextable-type table.
#' @param overall [\code{TRUE}] where to print overall model
#' @param ... arguments that are passed to sstable.survcomp
#'
#' @return a flextable-type table or a list with values/headers/footers
#'
#' @author This function was originally written by Marcel Wolbers. Trinh Dong and Lam Phung Khanh did some modification.
#' @import survival
#' @export
sstable.survcomp.subgroup <- function(base.model, subgroup.model, overall.model, data,
                                      time = Inf,
                                      reference.arm = c('B', 'A'),
                                      compare.method = c('cox', 'rmst', 'cuminc'),
                                      compare.args = list(),
                                      p.compare = TRUE,
                                      digits = 2, pdigits = 3, pcutoff = 0.001, footer = NULL, flextable = TRUE, bg = "#F2EFEE", overall = TRUE,...){


  requireNamespace("survival")

  ## strip the tibble class which causes issue - trinhdhk
  data <- as.data.frame(data)
  NAs <- model.frame(update(base.model, .~1), data=data) |> attr('na.action') |> unname()
  if (length(NAs)){
    warning(sprintf('Missing values on observation(s) %s',
                    paste(NAs,collapse=', ')))
    # data <- data[seq_len(nrow(data))[-NAs],]
    data <- dplyr::slice(data, seq_len(nrow(data))[-NAs])
  }
  compare.method <- match.arg(compare.method)
  fit.surv0 <- survfit(update(base.model, .~1), data=data)
  ms <- inherits(fit.surv0, 'survfitms')

  # arm.var <- if (length(base.model[[3]]) > 1) {
  #   deparse(base.model[[3]][[2]])
  # } else {
  #     deparse(base.model[[3]])
  #   }
  arm.var <- formula.tools::rhs.vars(base.model)[[1]]
  if (!inherits(data[, arm.var], "factor")) data[, arm.var] <- factor(data[, arm.var])

  # result in entire population
  if (!missing(overall.model)){
    added.terms <- terms(overall.model) |> attr('term.labels')
    overall.model <- update(base.model,
                            as.formula(paste('. ~ . +',
                                             paste(added.terms,
                                             collapse='+'))))
  } else overall.model <- base.model
  result <- sstable.survcomp(model = overall.model, data = data, time=time, reference.arm=reference.arm,
                             medsum = FALSE, digits = digits,
                             compare.method = compare.method, compare.args = compare.args,
                             p.compare = p.compare,
                             pdigits = 3, pcutoff = pcutoff, flextable = FALSE, ...)$table[,-1]
  result <- cbind(c("Subgroup", "", "All patients"), result, c("Test for heterogeneity", "p-value", ""))

  # Preparation of models and data
  subgroup.char <- all.vars(subgroup.model)

  #browser()
  for (k in 1:length(subgroup.char)){
    main.model <- update(base.model, as.formula(paste(". ~ . +", subgroup.char[k], sep = "")))
    ia.model <- update(base.model, as.formula(paste(". ~ . +`", arm.var,  "`*", subgroup.char[k], sep = "")))
    data$.subgroup.var <- data[, subgroup.char[k]]
    if (!inherits(data[, subgroup.char[k]], 'factor'))  data[, subgroup.char[k]] <- factor(data[, subgroup.char[k]])
    factor.levels <- levels(data[, subgroup.char[k]])
    compare.args.subgroup <- compare.args
    # Add interaction test for heterogeneity
    result <- rbind(result, "")
    result[nrow(result), 1] <- ifelse(is.null(attr(data[, subgroup.char[k]], "label")),
                                      subgroup.char[k], attr(data[, subgroup.char[k]], "label"))
    ia.pval <-
      if (compare.method == 'cox'){
        if (ms){
          etype <- compare.args$cause
          fg.ia <- survival::finegray(ia.model, data=data, etype=etype)
          fg.main <- survival::finegray(main.model, data=data, etype=etype)

          anova(
            survival::coxph(update(ia.model, Surv(fgstart,fgstop,fgstatus)~.),
                            data=fg.ia, weights=fgwt),
            survival::coxph(update(main.model, Surv(fgstart,fgstop,fgstatus)~.),
                            data=fg.main, weights=fgwt),
            test = "Chisq")[2, "Pr(>|Chi|)"]
        } else {
          anova(survival::coxph(ia.model, data = data),
                survival::coxph(main.model, data = data),
                test = "Chisq")[2, "Pr(>|Chi|)"]
        }
      } else {
        ia.args <- compare.args
        ia.args$add.prop.haz.test <- NULL
        # ia.args$formula <- ia.model
        ia.args$data <- data

        type <- if (is.null(ia.args$type)) 'diff' else ia.args$type
        ia.args$type <- NULL
        ia.args$link <- switch(type, "diff" = 'identity', "ratio" = 'log', 'lost.ratio' = 'log')
        ia.args$model.censoring <- if (is.null(ia.args$model.censoring)) 'stratified'
        ia.args$formula.censoring <-
          if (is.null(ia.args$formula.censoring)) update(main.model, NULL ~.) else
            update(ia.args$formula.censoring, as.formula(paste("NULL ~ . +", subgroup.char[k], sep = "")))
        mf <- model.frame(update(base.model, new = as.formula(paste0(". ~`", arm.var, '`'))), data = data)
        mf <- cbind(unclass(mf[,1]), mf[,2])
        # Get max of stop time, is the minimum of last observed time between two arms.
        # The stop time for right cens data is the first column ($time), for interval-cens data it is the second column ($stop)
        # With laziness it is coded as mf[, ncol(mf)-2] (the last two columns are the event status from Surv object, and the arm)
        # browser()
        minimax.time <- min(by(mf[, ncol(mf)-2], mf[, ncol(mf)], max))

        # tau for rmst is capped as minamax.time
        if (is.null(ia.args$time)) ia.args$time <- minimax.time
        else if (ia.args$time > minimax.time) {
          ia.args$time <- minimax.time
        }

        # Remove missing value from predictor bc this causes problem in censoring model
        NAs <- model.frame(main.model, data=data) |> attr('na.action') |> unname()
        if (length(NAs)){
          warning(sprintf('Missing %s on observation(s) %s',
                          subgroup.char[k],
                          paste(NAs,collapse=', ')))
          # data <- data[seq_len(nrow(data))[-NAs],]
          ia.args$data <- dplyr::slice(data, seq_len(nrow(data))[-NAs])
        }

        # browsere)
        # Perform multivariate wald test for interaction term
        tryCatch({
          fitter <- switch(compare.method,
                           'rmst' = eventglm::rmeanglm,
                           'cuminc' = eventglm::cumincglm)
          ia.fit <-
            do.call(fitter, append(ia.args, c(formula=ia.model)))
          main.terms <- colnames(model.matrix(main.model, data=ia.args$data))
          ia.terms <- coef(ia.fit)
          test.terms <- which(!names(ia.terms) %in% main.terms)
          test <- aod::wald.test(vcov(ia.fit), b=ia.terms, Terms=test.terms)
          test$result$chi2['P']
        },
        error=\(e) NA
        )

      }
      # browser()

    result[nrow(result), ncol(result)] <-
      if (is.na(ia.pval)) '-' else
        format.pval(ia.pval, digits = pdigits, eps = pcutoff)

    # Add results for each subgroup level
    for (j in 1:length(factor.levels)){
      result <- rbind(result, "")
      result[nrow(result), 1] <- paste("-", factor.levels[j])
      d.subgroup <- subset(data, .subgroup.var == factor.levels[j])
      # if (compare.method=='rmst'){
      #   compare.args$model.censoring <- if (is.null(compare.args$model.censoring)) 'stratified'
      #   compare.args$formula.censoring <-
      #     if (is.null(compare.args$formula.censoring)) update(main.model, NULL ~.) else
      #       update(compare.args$formula.censoring, as.formula(paste("NULL ~ . +", subgroup.char[k], sep = "")))
      # }
      # browser()
      result[nrow(result), 2:(ncol(result) - 1)] <- sstable.survcomp(model = base.model, data = d.subgroup, time=time,
                                                                     compare.method = compare.method, compare.args = compare.args,
                                                                     p.compare = p.compare,
                                                                     medsum = FALSE, digits = digits, pdigits = pdigits, pcutoff = pcutoff,
                                                                     flextable = FALSE, ...)$table[3,-1]
    }
  }
  ## footer
  compare.stat <- switch(compare.method,
                         cox = 'HR',
                         cuminc = if (is.null(compare.args$type)) 'Cumul.inc difference'
                         else switch(compare.args$type,
                                     diff = 'Cumul.inc difference', ratio = 'Cumul.inc ratio',
                                     stop('Illegal type for cumulative incidence comparison model')),
                         rmst = if (is.null(compare.args$type)) 'RMST difference'
                         else switch(compare.args$type,
                                     diff = 'RMST difference',
                                     lost.diff = 'RMTL difference',
                                     ratio = 'RMST ratio',
                                     lost.ratio = 'RMTL ratio',
                                     stop('Illegal type for RMST comparison model')))
  compare.note <- switch(
    compare.stat,
    'HR' = 'HR = hazard ratio',
    'Cumul.inc difference' = 'Cumul.inc : Cumulative incidence',
    'Cumul.inc ratio' = 'Cumul.inc : Cumulative incidence',
    'RMST difference' = 'RMST: Restricted mean survival time',
    'RMTL difference' = 'RMTL: Restricted mean time loss',
    'RMST ratio' = 'RMST: Restricted mean survival time',
    'RMTL ratio' = 'RMTL: Restricted mean time lost'
  )
  compare.name <- switch(compare.method,
                         cuminc='Generalized linear models for cumulative incidence',
                         cox=if (ms) 'Fine-Gray model' else 'Cox proportional hazards model',
                         rmst='Restricted mean survival time model')
  footer <- c(
    compare.note,
    paste(compare.stat, "and p value were based on", paste0(compare.name, '.')),
    sprintf(
      "Test for heterogeneity is an %s test for interaction between treatment effect and each subgroup in the survival model not including other variables.",
      if (compare.method == 'cox') 'Likelihood-ratio' else 'multivariate Wald'
    ),
    footer)

  # flextable
  if (!overall) result <- result[-3,]
  if (flextable) {
    requireNamespace("flextable")
    requireNamespace("officer")

    ## main table
    value <- result[-c(1, 2), ]
    tab <- flextable::flextable(as.data.frame(value))

    ## header
    header1 <- result[1, ]; header2 <- result[2, ]
    header2[1] <- header1[1]
    assign("tab",
           eval(parse(text = paste0("flextable::set_header_labels(tab,",
                                    paste(paste0("V", 1:length(header1)), paste0("'", header1, "'"), sep = "=", collapse = ","),
                                    ")"))))
    assign("tab",
           eval(parse(text = paste0("flextable::add_header(tab,",
                                    paste(paste0("V", 1:length(header1)), paste0("'", header2, "'"), sep = "=", collapse = ","),
                                    ", top = FALSE)"))))

    tab <- flextable::merge_v(tab, part = "header")

    ## footer
    if (any(value == "-")) footer <- c(footer, "- : value cannot be estimated.")
    for (k in (1:length(footer))) {
      tab <- flextable::add_footer(tab, V1 = footer[k], top = FALSE)
      tab <- flextable::merge_at(tab, i = k, j = 1:length(header1), part = "footer")
    }

    ## format
    ### width
    tab <- flextable::autofit(tab)
    ### alignment
    tab <- flextable::align(tab, j = 1, align = "left", part = "all")
    ### faces of header
    tab <- flextable::bold(tab, part = "header")
    ### background
    tab <- flextable::bg(tab, i = seq(from = 1, to = nrow(result[-c(1:2), ]), by = 2), j = 1:length(header1), bg = bg, part = "body")
    ### border
    tabbd <- officer::fp_border(color="black", width = 1.5)
    tab <- flextable::border_remove(tab)
    tab <- flextable::hline(tab, border = tabbd, part = "header")
    tab <- flextable::hline_top(tab, border = tabbd, part = "all")
    tab <- flextable::hline_bottom(tab, border = tabbd, part = "body")

  } else {
    tab <- list(table = result,
                footer = footer)
    class(tab$table) <- c('survcomp_tbl', 'ss_tbl', class(tab$table))
  }
  if (!flextable) class(tab) <- c('ss_survcomp','ss_obj')
  return(tab)
}

#' @export
print.ss_tbl <- function(sstable, pretty=getOption('ss_pretty.print', TRUE)){
  if (!pretty) return(print.default(sstable))
  tryCatch(
    {
      print(huxtable::as_hux(sstable) |> huxtable::theme_article())
    },
    error = function(e) {print.default(sstable)}
  )
  invisible(sstable)
}

