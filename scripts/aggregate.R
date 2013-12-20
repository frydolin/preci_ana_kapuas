####### PRECIPITATION ANALYSIS: COMPARISON OF GROUND DATA #######

## summaries.R creates all necessary summaries and variables to be analysed ## 
## see README for variable naming convention 

## aggregation with mean i.e.
## output is average daily rainfall per week, month, year, ... ##
## na.rm currently TRUE !!!!!!!

#### SET UP ####
  Sys.setlocale("LC_TIME", "en_US.UTF-8") #set up time locale to get english names 
  
  library("zoo")
  library("hydroTSM")
  source("scripts/functions.R") #own functions

  ## create plot output directory ##
  dir.create("output/files", recursive = FALSE)
### END SET UP ###

#### AGGREGATION OF DATA ####
#Daily
  #d_ts           #make time series
  #str(d_ts)      #check structure
  d_df=mdf(d_ts)  #make data frame for better plotting
  write.csv2(d_df, file="output/files/daily_data.csv", quote=F, na = "NA")


#Weekly
  w_ts <- lapply(d_ts, function(x) aggregate(x, by=as.Date(cut.Date(index(x), "7 day")), mean, na.rm=F))
  #str(w_ts)
  w_df=mdf(w_ts)
  write.csv2(w_df, file="output/files/weekly_data.csv", quote=F, na = "NA")

#Monthly
  m_ts <- lapply(d_ts, daily2monthly, mean, na.rm=TRUE)
  #str(m_ts)
  m_df=mdf(m_ts)
  write.csv2(m_df, file="output/files/monthly_data.csv", quote=F, na = "NA")


#Monthly average by Month i.e. average in all Januries, Febs, etc.
  davbm <- lapply(d_ts, monthlyfunction, mean, na.rm=TRUE)
  #str(davbm)
  davbm_df=mdf(davbm)
  write.csv2(davbm_df, file="output/files/monthly_average.csv", quote=F, na = "NA")


#Yearly
  y_ts <- lapply(d_ts, daily2annual, mean, na.rm=FALSE)
  y_df=mdf(y_ts)
  write.csv2(y_df, file="output/files/yearly_data.csv", quote=F, na = "NA")
  #sums
  ys_ts <- lapply(d_ts, daily2annual, sum, na.rm=FALSE)
  ys_df=mdf(ys_ts)
  write.csv2(ys_df, file="output/files/yearly_sums.csv", quote=F, na = "NA")

#Daily by Season
  #RS
  rs_ts<- lapply(d_ts, mextract, c(1,10:12))
  rs_df=mdf(rs_ts)

  #DS
  ds_ts <- lapply(d_ts, mextract, c(5:8))
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

# Daily to seasonal sum
  rsav_ts=lapply(d_ts, daily2season, season="RS", mean, na.rm=TRUE)
  rsav_df=mdf(rsav_ts)
  
  dsav_ts=lapply(d_ts, daily2season, season="DS", mean, na.rm=TRUE)
  dsav_df=mdf(dsav_ts)

### END AGGREGATION ###

#### Cumulative Daily Sums ####
  cumlist=lapply(d_ts, cumul) #make sums
  #cumfun_df_list=lapply(cumlist, function(x) lapply(x, unlistdo.call(cbind,unlist(x)) #doesnt work yet, convert to dataframe
  #write.csv2(cumfun_ts, file="output/files/cumulative_funct.xls", quote=F, na = "NA") oesnt work yet,
### END CUMULATIVE SUMS ###

#### Time Series by months i.e. Jan 1982, 1983, 1984,... Feb 1982, ... ####
  ## output is a list of station summaries, each containing 12 lists of monthly TS ##
  bymonth_ts=lapply(m_ts, ts.bymonth)
### END TIME SERIES BY MONTH ###

#### SOI AGGREGATION ####
  ysoi_ts=monthly2annual(soi_ts, mean)

########## END summaries.R #############
