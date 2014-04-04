###### SPATIO-TEMPORAL RAINFALL PATTERNS IN KAPUAS BASIN ######
	### ANALYSIS AND COMPARISON OF GAUGE DATA ###

## run.R:
## can be used to execute the whole analysis
## takes files in /input, analyzes them and writes the results to /output
## further information can be found in the README file

# Maybe you need to set the correct correct working directory: set.wd() or adjust file paths before analysis
# source("scripts/installpackages.R") ## checks if necessary packages are installed, if not installs them
source("scripts/load.R")	## loads in all the data from /input and converts it to appropriate formats ##
source("scripts/aggregate.R")	## creates all necessary aggregates, summaries and variables to be analyzed ##
source("scripts/analyse.R")	## plots/writes analysis and summaries to /output ##
source("scripts/compare.R")	## plots/writes comparative analysis and summaries to /output ##

###### END run.R ######
