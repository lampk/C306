##******************************************************************************
## Main analysis
## Author:
## Version:
##******************************************************************************


# setup -------------------------------------------------------------------

## set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## load functions
# source(file.path("functions", "xxx.R"))

## fake or real analysis
## (T for all analysis based on the dummy (fake) randomization list; set to F for the real analysis)
dummy <- TRUE

## import randomization list
randolist <- read.csv(file.path("..", "Data", "RandomizationList", "xxx.csv"))
arm_levels <- ifelse(dummy == TRUE,
                     paste("Dummy", LETTERS[1:length(unique(randolist$arm))]),
                     unique(randolist$arm))
randolist$arm <- factor(randolist$arm, levels = arm_levels)

## load data
load(file.path("..", "Data", "VAD", "xxx.Rdata"))


# analysis ----------------------------------------------------------------

## intermediate results/R objects should be saved in folder tmp
## figures should be saved in folder figures




