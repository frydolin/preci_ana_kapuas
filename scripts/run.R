###### PRECIPITATION ANALYSIS: COMPARISON OF STATION DATA ######

## run.R executes the analysis ##

# Maybe you need to set the correct correct wd!: set.wd()

source("scripts/load.R") 	    ## load.R loads in all the data and converts to appropriate formats ##
source("scripts/aggregate.R")	## summaries.R creates all necessary summaries and variables to be analysed ##
source("scripts/ts_plot.R")	  ## ts_plot.R plots summaries to output ##
source("scripts/sp_plot.R")   ## sp_plot.R plots comperative summaries to output ##

########## END run R#############
