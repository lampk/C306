##******************************************************************************
## Check and Clean imported data
## Author:
## Version:
##******************************************************************************

# set up ------------------------------------------------------------------

## set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## load imported data from file.path("..", "Data", "Imported")
load(file.path("..", "Data", "Imported", "xxx.Rdata"))

# check -------------------------------------------------------------------


# clean -------------------------------------------------------------------


# save --------------------------------------------------------------------

## save as .Rdata in file.path("..", "Data", "Cleaned")
save(..., file = file.path("..", "Data", "Cleaned", "xxx.Rdata"))
