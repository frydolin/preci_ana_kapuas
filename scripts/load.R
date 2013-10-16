###### PRECIPITATION ANALYSIS: COMPARISON OF GROUND DATA ######


#### LOAD IN ALL GROUND DATA ####
# Remember to setwd()
# Get file names
  fpath='gdata/'
  fnames = list.files(path=fpath, pattern="*.csv")
  files_tmp=paste(fpath,fnames, sep='')

#Read in
  gdata=lapply(files_tmp, read.csv, sep=";", dec=",", na.strings = "NA")
  
#Convert to date and numeric
  for (i in 1:length(gdata)) gdata[[i]]$date=as.Date(gdata[[i]]$date)
  for (j in 1:length(gdata)) gdata[[j]]$rain=as.numeric(gdata[[j]]$rain)
 
#rm
  rm(i,j)
  rm(files_tmp) 

# check
  str(gdata)
  fnames

### END LOAD DATA

#### CONVERT TO ZOO OBJECTS ####
  library(zoo)
  ts_gdata=lapply(gdata, function(x) zoo(x$rain, order.by=as.Date(x$date)))
  str(ts_gdata) #just to check

### END ZOO OBJECTS

