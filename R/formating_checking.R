convert.info <- function(oucru_info, oucru_category) {
  require(dplyr)
  require(car)
  ## get value & level for factors
  cat_tmp <- distinct(oucru_category) %>%
    mutate(database_value = ifelse(is.na(suppressWarnings(as.numeric(`Database value`))),
                                   paste("'", `Database value`, "'", sep = ""),
                                   `Database value`),
           title_en = paste("'", `Title EN`, "'", sep = "")) %>%
    group_by(`Category Code`) %>%
    summarise(value = paste(database_value, title_en, sep = "=", collapse = ";"),
              level = paste(title_en, collapse = ";")) %>%
    ungroup() %>%
    rename(`Value range` = `Category Code`)

  ## convert
  #browser()
  output <- merge(oucru_info, cat_tmp, by = "Value range", all.x = TRUE) %>%
    transmute(varname = `Field Name`,
              label   = `Title EN`,
              type    = Recode(`Data Type`,
                               recodes = "c('Category', 'ExCategory') = 'factor';
                               c('Free Text', 'Title', 'CombinedKey', 'Time', 'Check') = 'character';
                               c('EDateTime', 'SDateTime', 'DateTime', 'DateTime2', 'DateTime3') = 'datenumber';
                               c('Integer', 'Float') = 'numeric';
                               else = NA"),
              unit    = NA,
              value   = ifelse(is.na(type), NA,
                               ifelse(type == "factor", value,
                                      ifelse(type == "character", NA,
                                             ifelse(type == "datetime", "dmy_hms",
                                                    ifelse(type == "datenumber", "ymd",
                                                           ifelse(is.na(`Value range`), NA,
                                                                  ifelse(grepl(pattern = "-", x = `Value range`) == FALSE, paste(0, `Value range`, sep = ";"),
                                                                         gsub(pattern = "-", replacement = ";", x = `Value range`)))))))),
              levels  = ifelse(is.na(type), NA,
                               ifelse(type != "factor", NA, level)),
              missing = NA)
  return(subset(output, !is.na(varname)))
}

myformat.each <- function(x, label = NA, type, unit = NA, scale = NA, center = NA, value = NA, levels = NA, missing = NA){
  type <- tolower(type)
  ## missing data
  if (!is.na(missing)){
    missing <- gsub("['']", "", unlist(strsplit(as.character(missing), split = ";")))
    for (i in missing) x[x == i] <- NA
  }

  ## check type
  if (!type %in% c("numeric", "datetime", "datenumber", "character", "factor")){
    stop("This type of data is not implemented yet !!!")
  }

  ## numeric data
  if (type == "numeric"){
    tmp <- as.numeric(x)
    structure(tmp,
              class = c("avector", class(x)),
              label = ifelse(is.na(label), "", label),
              unit = ifelse(is.na(unit), "", unit),
              scale = ifelse(is.na(scale), 1, scale),
              center = ifelse(is.na(center), 0, center))
  }

  ## datetime data
  if (type == "datetime"){
    requireNamespace("lubridate")
    tmp <- eval(parse(text = paste("lubridate::", value, "(as.character(x))", sep = "")))
    structure(tmp, class = c("avector", class(tmp)), label = ifelse(is.na(label), "", label))
  }

  if (type == "datenumber"){
    tmp <- as.Date(as.numeric(x), origin = "1899-12-30")
    structure(tmp, class = c("avector", class(tmp)), label = ifelse(is.na(label), "", label))
  }

  ## character data
  if (type == "character"){
    x[x == ""] <- NA
    if (is.na(value)){tmp <- as.character(as.vector(x))} else {
      requireNamespace("car")
      tmp <- as.character(car::Recode(var = x, recodes = value, as.factor.result = FALSE))
      structure(tmp, class = c("avector", class(tmp)), label = ifelse(is.na(label), "", label))
    }
  }

  ## factor data
  if (type == "factor"){
    requireNamespace("car")
    if (is.na(levels)|levels == ""){
      tmp <- car::Recode(var = x, recodes = value, as.factor.result = TRUE)
    } else {
      tmp <- car::Recode(var = x, recodes = value, as.factor.result = TRUE,
                         levels = gsub("['']", "", unlist(strsplit(levels, split = ";"))))
    }
    structure(tmp, class = c("avector", class(tmp)), label = ifelse(is.na(label), "", label))
  }

  ## return
  return(tmp)
}

myformat.data <- function(data, info){
  # info should have these columns
  #- varname: name of variables
  #- label: label of variables
  #- type: type of variables (numeric, factor, character, datetime)
  #- unit: units of variables
  #- value: values of variables (format of datetime variables/values of categorical variables)
  #- levels: order of levels of categorical variables
  #- missing: coding for missing values

  ## recognize variable in data
  flag <- names(data)[names(data) %in% na.omit(info$varname)]
  if (length(flag) == 0){stop("No variable in this data present in info !!!")}
  info <- info[na.omit(match(flag, info$varname)),]

  ## select variables mentioned in info
  tmp <- data[, flag]

  ## if scale & center are missing --> using NA
  if (!"scale" %in% names(info)){info$scale <- NA}
  if (!"center" %in% names(info)){info$center <- NA}

  ## set type to lower case and no space
  info$type <- gsub(" ", "", tolower(info$type))

  ## perform formating
  return(data.frame(mapply(myformat.each,
                           x = as.list(tmp),
                           label = info$label,
                           type = info$type,
                           unit = info$unit,
                           scale = info$scale,
                           center = info$center,
                           value = info$value,
                           levels = info$levels,
                           missing = info$missing,
                           SIMPLIFY = FALSE),
                    stringsAsFactors = FALSE))
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

inspect.data <- function (data, info, id, check_missing, plot = FALSE, prefix = "", outdir) {
  if (missing(outdir))
    outdir <- "."
  data <- data[, names(data)[names(data) %in% info$varname]]
  info <- info[match(names(data), info$varname), ]
  if (missing(check_missing)) {
    info$check_missing <- TRUE
  }
  else {
    info$check_missing <- check_missing
  }
  output <- do.call("rbind", mapply(inspect.each, x = as.list(data),
                                    varname = info$varname, value = info$value, type = info$type,
                                    check_missing = info$check_missing))
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
      if (length(na.omit(data[, info$varname[i]])) == 0) {
        plot(x = 1:10, y = 1:10,
             main = paste(info$varname[i], "\n (", info$label[i], ")", " \n (", info$type[i], ")", sep = ""),
             type = "n", xlab = "", ylab = "")
        text(x = 5, y = 5, labels = "No non-missing value")
      }
      else {
        nNA <- sum(is.na(data[, info$varname[i]]))
        if (mode(data[, info$varname[i]]) != "numeric" |
            is.factor(data[, info$varname[i]])) {
          barplot(table(data[, info$varname[i]]),
                  main = paste(info$varname[i], "\n (", info$label[i], ")", " \n (", info$type[i], ", N=", N, ", missing=", nNA, ")", sep = ""))
        }
        else {
          if (is.POSIXct(data[, info$varname[i]])) {
            boxplot(as.Date(data[, info$varname[i]]),
                    main = paste(info$varname[i], "\n (", info$label[i], ")", " \n (", info$type[i], ", N=", N, ", missing=", nNA, ")", sep = ""))
          }
          else {
            boxplot(data[, info$varname[i]],
                    main = paste(info$varname[i], "\n (", info$label[i], ")", " \n (", info$type[i], ", N=", N, ", missing=", nNA, ")", sep = ""))
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
