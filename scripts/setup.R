###### SPATIO-TEMPORAL RAINFALL PATTERNS IN KAPUAS BASIN ######
	### ANALYSIS AND COMPARISON OF GAUGE DATA ###

## setup.R:
## loads important packages used in all scripts
## initializes variables used in all scripts
## called from within the analysis

## Attach base packages, usually unnecessary
#   library("utils")
#   library("stats")
#   library("class")
#   library("methods")
#   library("grDevices")
#   library("graphics")
   library("tools")

## Attach other often used packages (they might call further dependencies)
#   library("zoo")
#   library("xts")
  library("hydroTSM")

## Load own functions, as defined in the file.
  source("scripts/functions.R")

## Set up time locale to get English time format
  Sys.setlocale("LC_TIME", "en_US.UTF-8") 

###### END setup.R ######
