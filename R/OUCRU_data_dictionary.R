
# import OUCRU data dictionary --------------------------------------------

#' Import OUCRU's data dictionary into R
#'
#' @description A function to import OUCRU's data dictionary into R.
#'
#' @param table_name a character vector specifies names of Excel sheets (in OUCRU's data dictionary) to be imported.
#' @param input a character value specifies path to the OUCRU's data dictionary file (in Excel format).
#' @param output a character value specifies where to save imported data.
#'
#' @return Nothing (imported data will be saved in the pre-defined output directory)
#' @export
import.info <- function(table_name, input, output) {
  requireNamespace("readxl")

  ### get all available sheets
  crfs <- readxl::read_excel(path = input, sheet = "CRFs")
  ### check Grids table
  input_sheet <- readxl::excel_sheets(input)
  grid_id <- grep(pattern = "grids", x = input_sheet, ignore.case = TRUE)
  if (length(grid_id) > 0) {
    grids <- readxl::read_excel(path = input, sheet = grid_id)
    sheet_grid <- grids$sheet_grid <- with(grids, paste(CRF, Grid, sep = "_"))
  } else {
    sheet_grid <- NULL
  }
  sheet_final <- intersect(unique(c(crfs$CRF, sheet_grid)),
                           table_name)

  ### import
  #### no-grid sheets
  for (i in sheet_final[!sheet_final %in% sheet_grid] ) {
    assign(paste(i, "info", sep = "_"),
           subset(readxl::read_excel(path = input, sheet = i), (!`Data type` %in% "Title")))
  }
  #### grid sheets
  #browser()
  if (!is.null(sheet_grid)) {
    grids <- grids[grids$sheet_grid %in% sheet_final, ]
    for (i in (1:nrow(grids))) {
      tmp <- rbind(subset(get(paste(grids$CRF[i], "info", sep = "_")),
                          Grid == grids$Grid[i]),
                   c(paste(grids$CRF[i], grids$Grid[i], "SEQ", sep = "_"), "", "Number", rep("", 5)),
                   c("USUBJID", "Unique subject id", "Free Text", rep("", 5)))
      assign(paste(grids$CRF[i], grids$Grid[i], "info", sep = "_"),
             tmp)
    }
  }
  #### all info-sheet names
  sheet_info_name <- paste(sheet_final, "info", sep = "_")

  ### save
  save(list = c(sheet_info_name, "sheet_info_name"), file = output)
}

# convert OUCRU’s data dictionary to a general data dictionary -----------------

#' Convert imported OUCRU's data dictionary into a general data dictionary
#'
#' @description A function to convert imported OUCRU's data dictionary into a general data dictionary.
#'
#' @param oucru_info a data frame contains imported data of a sheet from OUCRU's data dictionary.
#' @param oucru_category a data frame specifies how categorical variables are coded in OUCRU's database.
#'
#' @return A data frame contains data dictionary information in a general format.
#' @import dplyr
#' @export
convert.info <- function(oucru_info, oucru_category) {
  requireNamespace("dplyr")
  requireNamespace("car")

  ## get value & level for factors
  cat_tmp <- distinct(oucru_category) %>%
    rename(Category = category) %>%
    mutate(submissionvalue = ifelse(is.na(suppressWarnings(as.numeric(submissionvalue))),
                                    paste0("'", submissionvalue, "'", sep = ""),
                                    submissionvalue),
           text = paste0("'", gsub(pattern = "=", replacement = " equal ",
                                   x = gsub(pattern = "[\x01-\x1f\x7f-\xff:]", replacement = "",
                                            x = gsub(pattern = ";", replacement = ",", x = text))), "'")) %>%
    group_by(Category) %>%
    summarise(value = paste(submissionvalue, text, sep = "=", collapse = ";"),
              level = paste(text, collapse = ";")) %>%
    ungroup()

  ## convert
  output <- merge(oucru_info, cat_tmp, by = "Category", all.x = TRUE) %>%
    mutate(Format = tolower(Format)) %>%
    transmute(varname = Variable,
              label   = Caption,
              type    = car::Recode(`Data type`,
                               recodes = "c('Category', 'ExCategory', 'RadioList') = 'factor';
                               c('Free Text', 'Title', 'CombinedKey', 'Time', 'Check', 'Other') = 'character';
                               c('EDateTime', 'SDateTime', 'DateTime', 'DateTime2', 'DateTime3') = 'datetime';
                               c('Integer', 'Float', 'Number') = 'numeric';
                               else = NA"),
              unit    = NA,
              value   = ifelse(is.na(type), NA,
                               ifelse(type == "factor", value,
                                      ifelse(type == "character", NA,
                                             ifelse(type == "datetime" & Format %in% c("dd/mm/yy"), "ymd_hms",
                                                    ifelse(type == "datetime" & Format %in% c("hh:mm"), "ymd_hms",
                                                           ifelse(type == "numeric", NA, NA)))))),
              levels  = ifelse(is.na(type), NA,
                               ifelse(type != "factor", NA, level)),
              missing = NA,
              scale = NA,
              condition = NA,
              strict = "Yes")
  return(subset(output, !is.na(varname)))
}
