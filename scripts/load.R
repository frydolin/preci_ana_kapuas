###### SPATIO-TEMPORAL RAINFALL PATTERNS IN KAPUAS BASIN ######
	### ANALYSIS AND COMPARISON OF GAUGE DATA ###

## load.R 
## loads in all the data and converts it to appropriate format

### SET UP ###
	source("scripts/setup.R")
###

#### LOAD IN ALL GROUND DATA ####
# You need to be in the correct working directory 

# Get file names
	fpath='input/'
	fnames = list.files(path=fpath, pattern="[0-9]+.+.csv+", full.names=TRUE)
  
# Read in data, output is a list where each entry contains the data of one station
	# make sure input file uses same seperator, comma and missing data sign
	gdata=lapply(fnames, read.csv, sep=";", dec=",", na.strings = "NA")

# Convert format of entries to date and numeric
  # please note the structure of the input files as described in the README
  # or modify the code here and later accordingly
  for (i in 1:length(gdata)) gdata[[i]]$date=as.Date(gdata[[i]]$date)
  for (j in 1:length(gdata)) gdata[[j]]$rainfall=as.numeric(gdata[[j]]$rainfall)
### END LOAD DATA ###

#### MAKE STATION NAME LIST ####
# needs to be modified if file names convention changes
  stnames<-basename(file_path_sans_ext(fnames)) # get filename without extension and path
  stnames<-toupper(stnames) # convert to uppercase
  stnames<-substr(stnames,3, length(stnames)) # get rid of prefix ordering numbers
### END STATION NAME LIST ###

#### CONVERT TO ZOO (TIME SERIES) OBJECTS ####
  d_ts=lapply(gdata, function(x) zoo(x$rain, order.by=as.Date(x$date)))
  names(d_ts)=stnames
  # create additional "dummy" timeseries, for example for labeling
  dummy=d_ts[[1]]
  dummy[1:length(dummy)]=1
### END CONVERT TO ZOO OBJECTS ###

#### PRESELECTION OF STATIONS ####
	# As not all stations are used for each application, here stations can be excluded from the analysis
	# !Also the color vector in graphic_pars.R needs to be adjusted!
# For statistical analysis
#   d_ts=d_ts[c(-3,-8,-9,-11,-13,-14)]
#   stnames=stnames[c(-3,-8,-9,-11,-13,-14)]
# For spatial interpolation
#   d_ts=d_ts[c(-8,-9,-13)]
#   stnames=stnames[c(-8,-9,-13)]
### END PRESELECTION ###

#### CLEAN UP ####
# remove variables and data not needed anymore
  rm(i,j)
  rm(fnames, fpath) 
  rm(gdata)
### END CLEAN UP ###

###### END load.R ######
