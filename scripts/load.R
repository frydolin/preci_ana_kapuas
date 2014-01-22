###### SPATIO-TEMPORAL RAINFALL PATTERNS IN KAPUAS BASIN ######
### Analysis and comparison of station data ###

## load.R loads in all the data and converts to appropriate formats ##

### SET UP ###
source("scripts/setup.R")
###

#### LOAD IN ALL GROUND DATA ####
# You need to be in the correct working directory 

# Get file names
  fpath='input/'
  fnames = list.files(path=fpath, pattern="[0-9]+.+.csv+", full.names=TRUE)
  
# Read in
  gdata=lapply(fnames, read.csv, sep=";", dec=",", na.strings = "NA")

# Convert format to date and numeric
  # please not the structure of the input files as described in the README, or
  # modify the code here and later accordingly
  for (i in 1:length(gdata)) gdata[[i]]$date=as.Date(gdata[[i]]$date)
  for (j in 1:length(gdata)) gdata[[j]]$rainfall=as.numeric(gdata[[j]]$rainfall)

### END LOAD DATA ###

#### MAKE STATION NAME LIST ####
# needs to be modified if file names convention changes
  stnames<-basename(file_path_sans_ext(fnames)) #get filename without extension and path
  stnames<-toupper(stnames) # convert to uppercase
  stnames<-substr(stnames,3, length(stnames)) # get rid of prefix ordering numbers
### END STATION NAME LIST ###

#### CONVERT TO ZOO (TIME SERIES) OBJECTS ####
  d_ts=lapply(gdata, function(x) zoo(x$rain, order.by=as.Date(x$date)))
  #str(d_ts) #just to check
### END ZOO OBJECTS ###

# #### LOAD ENSO DATA ####
#   soi=read.csv("input/soi.csv", sep=";", dec=",", na.strings = "NA")
#   soi_ts=zoo(soi$soi, order.by=as.Date(soi$date))
# ### END LOAD ENSO DATA ###

#### CLEAN UP ####
# remove variables and data not needed anymore
  rm(i,j)
  rm(fnames, fpath) 
  rm(soi, gdata)
### END CLEAN UP ###

##### END load.R ######
