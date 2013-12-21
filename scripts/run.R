###### PRECIPITATION ANALYSIS: COMPARISON OF STATION DATA ######

## run.R executes the analysis ##

# Maybe you need to set the correct correct wd!: set.wd()

source("scripts/load.R") 	    ## loads in all the data and converts to appropriate formats ##
source("scripts/aggregate.R")	## creates all necessary summaries and variables to be analysed ##
source("scripts/analyse.R")	  ## plots summaries to output ##
source("scripts/compare.R")   ## plots comparative summaries to output ##

########## END run R#############
