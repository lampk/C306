##******************************************************************************
## Create value-added data for analysis
## Author:
## Version:
##******************************************************************************

# set up ------------------------------------------------------------------

## set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## load imported data from file.path("..", "Data", "Cleaned")
load(file.path("..", "Data", "Cleaned", "xxx.Rdata"))

# format ------------------------------------------------------------------


# manipulate --------------------------------------------------------------


# save --------------------------------------------------------------------

## save as .Rdata in file.path("..", "Data", "VAD")
save(..., file = file.path("..", "Data", "VAD", "xxx.Rdata"))


