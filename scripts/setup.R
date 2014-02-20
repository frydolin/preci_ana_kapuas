###### SPATIO-TEMPORAL RAINFALL PATTERNS IN KAPUAS BASIN ######
### Analysis and comparison of station data ###

## setup.R:
## loads important packages used in all scripts
## initializes variables used in all scripts

#Attach base packages, usually unnecessary
  library("utils")
  library("stats")
  library("class")
  library("methods")
  library("grDevices")
  library("graphics")
  library("tools")

#Attach other often used packages (they might call further dependencies)
  library("zoo")
  library("xts")
  library("hydroTSM")

## load own functions, as defined in the file.
  source("scripts/functions.R")

## set up time locale to get English time format
  Sys.setlocale("LC_TIME", "en_US.UTF-8") 

###### END SET UP ######