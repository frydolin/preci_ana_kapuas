####### PRECIPITATION ANALYSIS: COMPARISON OF GROUND DATA #######

## summaries.R creates all necessary summaries and variables to be analysed ## 
## see README for variable naming convention 

## aggregation with mean i.e.
## output is average daily rainfall per week, month, year, ... ##

#### SET UP ####
  Sys.setlocale("LC_TIME", "en_US.UTF-8") #set up time locale to get english names 
  
  library("zoo")
  library("hydroTSM")
  source("scripts/functions.R") #own functions
### END SET UP ###

#### MAKE SUMMARIES ####
#Daily
  #d_ts           #make time series
  #str(d_ts)      #check structure
  d_df=mdf(d_ts)  #make data frame for better plotting

#Weekly
  w_ts <- lapply(d_ts, function(x) aggregate(x, by=as.Date(cut.Date(index(x), "7 day")), mean, na.rm=F))
  #str(w_ts)
  w_df=mdf(w_ts)
  
#Monthly
  m_ts <- lapply(d_ts, daily2monthly, mean, na.rm=F)
  #str(m_ts)
  m_df=mdf(m_ts)

#Monthly average by Month i.e. average in all Januries, Febs, etc.
  davbm <- lapply(d_ts, monthlyfunction, mean)
  #str(davbm)
  davbm_df=mdf(davbm)

#Yearly
  y_ts <- lapply(d_ts, daily2annual, mean, na.rm=F)
  #str(y_ts)
  y_df=mdf(y_ts)

#Daily by Season
  #RS
  rs_ts <- lapply(d_ts, function(x) x[as.numeric(format.Date(time(x), "%m")) %in% c(1,10,11,12)]) 
  #str(rs_ts)
  rs_df=mdf(rs_ts)

  #DS
  ds_ts <- lapply(d_ts, function(x) x[as.numeric(format.Date(time(x), "%m")) %in% c(5,6,7,8)]) 
  #str(ds_ts)
  ds_df=mdf(ds_ts)

#Monthly by Season
  #RS
  mrs_ts<- lapply(rs_ts, daily2monthly, mean, na.rm=F)
  #str(m_ts_gdata)
  mrs_df=mdf(mrs_ts)

  #DS
  mds_ts<- lapply(ds_ts, daily2monthly, mean, na.rm=F)
  #str(mds_ts)
  mds_df=mdf(mds_ts)

### END MAKE SUMMARIES ###

# #### Cumulative Daily Sums ####
#   cumlist=lapply(d_ts, cumul) #make sums
#   cumfun_ts=lapply(cumlist, function(x) zoo(x, order.by=time(d_ts[[1]]))) #convert to zoo
#   cumfun_df=mdf(cumfun_ts) #convert to dataframe
#   #write.csv2(cumfun_ts, file="output/files/cumulative_funct.xls", quote=F, na = "NA")
# ### END CUMULATIVE SUMS ###

#### Time Series by months i.e. Jan 1982, 1983, 1984,... Feb 1982, ... ####
  ## output is a list of station summaries, each containing 12 lists of monthly TS ##
  bymonth_ts=lapply(m_ts, ts.bymonth)
### END TIME SERIES BY MONTH ###

########## END summaries.R #############
