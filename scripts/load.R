###### PRECIPITATION ANALYSIS: COMPARISON OF GROUND DATA ######

## load.R loads in all the data and converts to appropriate formats ##

### SET UP ###
source("scripts/setup.R")
###

#### LOAD IN ALL GROUND DATA ####
# You need to be in the correct working directory 

# Get file names
  fpath='input/'
  fnames = list.files(path=fpath, pattern="[0-9]+.+.csv+")
  files_tmp=paste(fpath,fnames, sep='')

# Read in
  gdata=lapply(files_tmp, read.csv, sep=";", dec=",", na.strings = "NA")
  
# Convert format to date and numeric
  for (i in 1:length(gdata)) gdata[[i]]$date=as.Date(gdata[[i]]$date)
  for (j in 1:length(gdata)) gdata[[j]]$rainfall=as.numeric(gdata[[j]]$rainfall)

### END LOAD DATA ###

#### MAKE CORRECT STATION NAME LIST ####
#!! needs to be modified if file names convention changes
  stnames<-substr(toupper(fnames), 3,7) 
### END STATION NAME LIST ###

#### CONVERT TO ZOO (TIME SERIES) OBJECTS ####
  d_ts=lapply(gdata, function(x) zoo(x$rain, order.by=as.Date(x$date)))
  #str(d_ts) #just to check
### END ZOO OBJECTS ###

#### LOAD ENSO DATA ####
  soi=read.csv("input/soi.csv", sep=";", dec=",", na.strings = "NA")
  soi_ts=zoo(soi$soi, order.by=as.Date(soi$date))
### END LOAD ENSO DATA ###

#### CLEAN UP ####
# remove variables and data not needed anymore
  rm(i,j)
  rm(files_tmp, fpath) 
  rm(soi, gdata)
### END CLEAN UP ###

########## END load.R #############
