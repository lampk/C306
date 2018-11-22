
# find, match and correct variable names ----------------------------------

find_match_correct_var <- function(x, realvar) {
  if (is.atomic(x) || is.name(x)) {
    x
  } else {
    if (is.call(x)) {
      if (identical(x[[1]], quote(`==`)) | identical(x[[1]], quote(`is.na`)) | identical(x[[1]], quote(`%in%`)) | identical(x[[1]], quote(`as.numeric`))) {
        id <- na.omit(match(tolower(x[[2]]), tolower(realvar)))
        if (length(id) > 0) {
          x[[2]] <- parse(text = realvar[id])[[1]]
          x
        }
      } else {
        as.call(lapply(x, find_match_correct_var, realvar = realvar))
      }
    } else {
      as.call(lapply(x, find_match_correct_var, realvar = realvar))
    }
  }
}

find_match_correct_var_final <- function(cond, realvar) {
  out <- deparse(find_match_correct_var(parse(text = cond)[[1]], realvar = realvar), width.cutoff = 500L)
  return(gsub(pattern = "[\"]", replacement = "'", x = out))
}


# check data --------------------------------------------------------------

#' Check formatted data based on a pre-defined information
#'
#' @description A function to check a formatted data frame based on a pre-defined information.
#'
#' @param data a formatted data frame to be checked.
#' @param info a data frame specifies how variables will be formatted. This data frame should have the following columns: varname (character variable specifies name of each variable), label (character variable specifies label of each variables), type (character variable specifies type of each variable [numeric, factor, character, datetime], unit (character variable specifies units of each continuous variable), value (character variable specifies values of each variable [format of datetime variables/values of categorical variables], levels (character variable specifies order of levels of each categorical variable), missing (character variable specifies coding for missing values for each variable), condition (character variable specifies conditional checking), strict (a character variable [Yes, No] specifies whether missing data should be checked for each variable).
#' @param id a character specifies name of subject id variable.
#' @param check_missing a logical value specifies whether missing data should be checked.
#' @param plot a logical value specifies whether plot (bar plot for categorical variables, boxplot for continuous variables) should be produced.
#' @param prefix a character to be appended as prefix of all output files.
#' @param outdir a character specifies where to save output files.
#'
#' @return A data frame lists all identified potential data errors.
#' @export
inspect.data <- function (data, info, id, check_missing = c(TRUE, FALSE), plot = FALSE, prefix = "", outdir) {
  requireNamespace("lubridate")
  if (missing(outdir))
    outdir <- "."
  data <- data[, names(data)[tolower(names(data)) %in% tolower(info$varname)]]
  info <- info[match(tolower(names(data)), tolower(info$varname)), ]

  if (missing(check_missing) | (!missing(check_missing) & check_missing == TRUE)) {
    info$check_missing <- TRUE

    if ("strict" %in% names(info)) {
      info$check_missing <- ifelse(info$strict == "Yes", TRUE, FALSE)
    }
  }
  else {
    info$check_missing <- FALSE
  }
  #browser()
  output <- do.call("rbind",
                    lapply(X = 1:ncol(data),
                           FUN = function(x) {
                             tmpdata <- data[, x]
                             if ("condition" %in% names(info)) {
                               if (!is.na(info$condition[x])) {
                                 ## match variable name is condition
                                 tmpcondition <- find_match_correct_var_final(cond = info$condition[x], realvar = names(data))
                                 tmpdata <- eval(parse(text = paste0("subset(data,", tmpcondition, ")[, x]")))
                               }
                             }
                             inspect.each(x = tmpdata,
                                          varname = info$varname[x],
                                          value = info$value[x],
                                          type = info$type[x],
                                          check_missing = info$check_missing[x])
                           }))

  if (!missing(id) & !is.null(output)) {
    output[, id] <- paste("\"=\"\"", as.character(data[output$index,
                                                       id]), "\"\"\"", sep = "")
    output <- output[, c(id, "index", "error")]
  }

  if (plot) {
    pdf(file = file.path(outdir, paste(prefix, "distribution.pdf",
                                       sep = "_")), width = 12, height = 10, family = "Helvetica",
        fonts = NULL, paper = "a4r")
    layout(matrix(c(1:12), ncol = 4, nrow = 3, byrow = TRUE),
           respect = TRUE)
    N <- nrow(data)
    for (i in (1:length(info$varname))) {
      tmpdata <- data[, names(data)[tolower(names(data)) == tolower(info$varname[i])]]
      tmplabel <- substr(gsub(pattern = "[\x01-\x1f\x7f-\xff:]", replacement = "",
                              x = info$label[i]), start = 1, stop = 30)
      if (length(na.omit(tmpdata)) == 0) {
        plot(x = 1:10, y = 1:10,
             main = paste(info$varname[i], "\n (", tmplabel, ")", " \n (", info$type[i], ")", sep = ""),
             type = "n", xlab = "", ylab = "")
        text(x = 5, y = 5, labels = "No non-missing value")
      }
      else {
        nNA <- sum(is.na(tmpdata))
        if (mode(tmpdata) != "numeric" |
            is.factor(tmpdata)) {
          x <- barplot(table(tmpdata),
                       main = paste(info$varname[i], "\n (", tmplabel, ")", " \n (", info$type[i], ", N=", N, ", missing=", nNA, ")", sep = ""),
                       xaxt = "n")
          labs <- substr(gsub(pattern = "[\x01-\x1f\x7f-\xff:]", replacement = "",
                              x = names(table(tmpdata))), start = 1, stop = 10)
          text(cex = 1, x = x, y = 0, labs, xpd = TRUE, srt = 45, adj = 1)
        }
        else {
          if (lubridate::is.POSIXct(tmpdata)) {
            boxplot(as.Date(tmpdata),
                    main = paste(info$varname[i], "\n (", tmplabel, ")", " \n (", info$type[i], ", N=", N, ", missing=", nNA, ")", sep = ""))
          }
          else {
            boxplot(tmpdata,
                    main = paste(info$varname[i], "\n (", tmplabel, ")", " \n (", info$type[i], ", N=", N, ", missing=", nNA, ")", sep = ""))
          }
        }
      }
    }
    dev.off()
  }
  if (!is.null(output)) {
    write.csv(output, file = file.path(outdir, paste(prefix,
                                                     "error.csv", sep = "_")), quote = FALSE, row.names = FALSE)
    return(output)
  }
  else {
    cat("No error was found !")
  }
}

inspect.each <- function (x, varname, type, value = NA, check_missing = TRUE) {
  tmp <- NULL
  if (check_missing) {
    if (anyNA(x) | any(as.character(x) == "")) {
      tmp <- rbind(tmp, data.frame(index = which(is.na(x) |
                                                   as.character(x) == ""), error = paste("Missing value for",
                                                                                         varname)))
    }
  }
  if (type == "numeric" & (!is.na(value) & value != "")) {
    range <- as.numeric(unlist(strsplit(value, split = ";")))
    if (any(x < range[1] & !is.na(x))) {
      tmp <- rbind(tmp, data.frame(index = which(!is.na(x) &
                                                   x < range[1]), error = paste("Out of range:",
                                                                                varname, "<", range[1])))
    }
    if (any(x > range[2] & !is.na(x))) {
      tmp <- rbind(tmp, data.frame(index = which(!is.na(x) &
                                                   x > range[2]), error = paste("Out of range:",
                                                                                varname, ">", range[2])))
    }
  }
  if (type == "factor" & !is.na(value)) {
    range <- sapply(unlist(strsplit(value, split = ";")),
                    function(x) gsub("[']", "", unlist(strsplit(x, split = "="))[2]),
                    USE.NAMES = FALSE)
    if (any(!x %in% range & !is.na(x))) {
      xx <- x[!is.na(x) & !x %in% range]
      tmp <- rbind(tmp, data.frame(index = which(!is.na(x) &
                                                   !x %in% range), error = paste("Out of range:",
                                                                                 varname, "=", xx)))
    }
  }
  return(tmp)
}
