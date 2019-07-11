##******************************************************************************
## Import raw data into R
## Author:
## Version:
##******************************************************************************

# set up ------------------------------------------------------------------

## set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# import ------------------------------------------------------------------

## import raw data from file.path("..", "Data", "Raw", "Final")

## import metadata from file.path("..", "Data", "Info")

# save --------------------------------------------------------------------

## save as .Rdata in file.path("..", "Data", "Imported")
save(..., file = file.path("..", "Data", "Imported", "xxx.Rdata"))


