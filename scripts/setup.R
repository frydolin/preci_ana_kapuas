###### RAINFALL ANALYSIS: COMPARISON OF GROUND DATA ######

## setup.R:
## loads important packages used in all scripts
## initializes variables used in all scripts

#Attach base packages
  library("utils")
  library("stats")
  library("class")
  library("methods")
  library("grDevices")
  library("graphics")

#Attach other often used packages (they might call further dependencies)
  library("zoo")
  library("xts")
  library("hydroTSM")

## load own functions, as defined in the file.
  source("scripts/functions.R")

## set up time locale to get english names 
  Sys.setlocale("LC_TIME", "en_US.UTF-8") 

## COLOR SCHEME for plots##
# repeats these 12 color values 3 times -> 36 color values
  hexcolors=rep(c("#222222",
                  "#0EC20E",
                  "#A30008",
                  "#7F054A",
                  "#00FFFF",
                  "#6D14A2",
                  "#17599E",
                  "#057E05",
                  "#F31D11",
                  "#0B9B7F",
                  "#F36A11",
                  "#FF00FF"), 3)

####### END SET UP ######