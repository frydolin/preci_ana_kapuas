####### PRECIPITATION ANALYSIS: COMPARISON OF GROUND DATA #######

## summaries.R creates all necessary summaries and variables to be analysed ##

## see README for variable naming convention ##

#### MAKE SUMMARIES ####
library("hydroTSM")
source("scripts/functions.R") #functions such as mdf, . As defined in the file.

#Daily
  ts_gdata
  str(ts_gdata)
  df_gdata=mdf(ts_gdata)

#Weekly
  wts_gdata <- lapply(ts_gdata, function(x) aggregate(x, by=as.Date(cut.Date(index(x), "7 day")), sum, na.rm=F))
  str(wts_gdata)
  w_gdata=mdf(wts_gdata)
  
#Monthly
  mts_gdata <- lapply(ts_gdata, daily2monthly, sum, na.rm=F)
  str(mts_gdata)
  m_gdata=mdf(mts_gdata)

#Monthly average by Month
  mav <- lapply(ts_gdata, monthlyfunction, mean)
  str(mav)
  df_mav=mdf(mav)

#Yearly
  yts_gdata <- lapply(ts_gdata, daily2annual, sum, na.rm=F)
  str(yts_gdata)
  y_gdata=mdf(yts_gdata)

#Daily by Season
  #RS
  rsts_gdata <- lapply(ts_gdata, function(x) x[as.numeric(format.Date(time(x), "%m")) %in% c(1,10,11,12)]) 
  str(rsts_gdata)
  rs_gdata=mdf(rsts_gdata)

  #DS
  dsts_gdata <- lapply(ts_gdata, function(x) x[as.numeric(format.Date(time(x), "%m")) %in% c(5,6,7,8)]) 
  str(dsts_gdata)
  ds_gdata=mdf(dsts_gdata)

#Monthly by Season
  #RS
  mrsts_gdata<- lapply(rsts_gdata, daily2monthly, sum, na.rm=F)
  str(mts_gdata)
  mrs_gdata=mdf(mrsts_gdata)

  #DS
  mdsts_gdata<- lapply(dsts_gdata, daily2monthly, sum, na.rm=F)
  str(mts_gdata)
  mds_gdata=mdf(mdsts_gdata)

########## END #############
