myformat.each <- function(x, label = NA, type, unit = NA, scale = NA, center = NA, value = NA, levels = NA, missing = NA){
  type <- tolower(type)
  ## missing data
  if (!is.na(missing)){
    missing <- gsub("['']", "", unlist(strsplit(as.character(missing), split = ";")))
    for (i in missing) x[x == i] <- NA
  }

  ## check type
  if (!type %in% c("numeric", "datetime", "character", "factor")){
    stop("This type of data is not implemented yet !!!")
  }

  ## numeric data
  if (type == "numeric"){
    tmp <- as.numeric(x)
    tmp <- structure(tmp,
                     label = ifelse(is.na(label), "", label),
                     unit = ifelse(is.na(unit), "", unit),
                     scale = ifelse(is.na(scale), 1, scale),
                     center = ifelse(is.na(center), 0, center))
  }

  ## datetime data
  if (type == "datetime"){
    requireNamespace("lubridate")
    tmp <- eval(parse(text = paste("lubridate::", value, "(as.character(x))", sep = "")))
    tmp <- structure(tmp, label = ifelse(is.na(label), "", label))
  }

  ## character data
  if (type == "character"){
    x[x == ""] <- NA
    if (is.na(value)){tmp <- as.character(as.vector(x))} else {
      requireNamespace("car")
      tmp <- as.character(car::Recode(var = x, recodes = value, as.factor = FALSE))
      tmp <- structure(tmp, label = ifelse(is.na(label), "", label))
    }
  }

  ## factor data
  if (type == "factor"){
    requireNamespace("car")
    #browser()
    if (is.na(levels)|levels == ""){
      tmp <- car::Recode(var = x, recodes = value, as.factor = TRUE)
    } else {
      tmp <- car::Recode(var = x, recodes = value, as.factor = TRUE,
                         levels = gsub("['']", "", unlist(strsplit(levels, split = ";"))))
    }
    tmp <- structure(tmp, label = ifelse(is.na(label), "", label))
  }

  ## return
  return(tmp)
}

#' @export
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
  allvars <- tolower(names(data))
  flag <- names(data)[tolower(names(data)) %in% tolower(na.omit(info$varname))]
  if (length(flag) == 0){stop("No variable in this data present in info !!!")}
  info <- info[na.omit(match(tolower(flag), tolower(info$varname))),]

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
