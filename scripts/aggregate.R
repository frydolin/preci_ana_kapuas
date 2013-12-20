####### PRECIPITATION ANALYSIS: COMPARISON OF GROUND DATA #######

## aggregate.R creates all necessary summaries and variables to be analysed
## and outputs summary tables 
## see README for variable naming convention 

## aggregation with mean i.e. output is average daily rainfall per week, month, year, ... ##
## and also sums ##
## na.rm currently FALSE 

#### SET UP ####
  Sys.setlocale("LC_TIME", "en_US.UTF-8") #set up time locale to get english names 
  library("zoo")
  library("hydroTSM")
  source("scripts/functions.R") #own functions
  ## create plot output directory ##
  dir.create("output/files", recursive = FALSE)
  dir.create("output/seasonality", recursive = FALSE)
### END SET UP ###

#### TIME SERIES AGGREGATION ####
#Daily
  #d_ts           #make time series
  #str(d_ts)      #check structure
  d_df=mdf(d_ts, coln=stnames)  #make data frame for better plotting
  write.csv(d_df, file="output/files/daily_data.csv", na = "NA")

#Weekly
  #mean
  w_ts <- lapply(d_ts, function(x) aggregate(x, by=as.Date(cut.Date(index(x), "7 day")), mean, na.rm=FALSE))
  #str(w_ts)
  w_df=mdf(w_ts, coln=stnames)
  write.csv(w_df, file="output/files/weekly_means.csv", na = "NA")
  #sum  
  ws_ts <- lapply(d_ts, function(x) aggregate(x, by=as.Date(cut.Date(index(x), "7 day")), sum, na.rm=FALSE))
  ws_df=mdf(w_ts, coln=stnames)
  write.csv(ws_df, file="output/files/weekly_sums.csv", na = "NA")

#Monthly
  #means
  m_ts <- lapply(d_ts, daily2monthly, mean, na.rm=FALSE)
  m_df=mdf(m_ts, coln=stnames)
  write.csv(m_df, file="output/files/monthly_means.csv", na = "NA")
  #sums
  ms_ts <- lapply(d_ts, daily2monthly, sum, na.rm=FALSE)
  ms_df=mdf(m_ts, coln=stnames)
  write.csv(ms_df, file="output/files/monthly_sums.csv", na = "NA")

#Yearly
  y_ts <- lapply(d_ts, daily2annual, mean, na.rm=FALSE)
  y_df=mdf(y_ts, coln=stnames)
  write.csv(y_df, file="output/files/yearly_means.csv", na = "NA")
  #sums
  ys_ts <- lapply(d_ts, daily2annual, sum, na.rm=FALSE)
  ys_df=mdf(ys_ts, coln=stnames)
  write.csv(ys_df, file="output/files/yearly_sums.csv", na = "NA")
  
### END TS AGGREGATION ###

#### AGGREGATION BY MONTH ####
#Long term daily average by Month 
#i.e. daily average in all Januries, Febs, etc.
  davbm <- lapply(d_ts, monthlyfunction, mean, na.rm=TRUE)
  #str(davbm)
  davbm_df=mdf(davbm, coln=stnames)
  write.csv(davbm_df, file="output/files/bymonth_dailymean.csv", na = "NA")

#Long term monthly average by Month 
#i.e. monthly average of all Januries, Febs, etc.
# input are monthly sums for all months
  mavbm <- lapply(ms_ts, monthlyfunction, mean, na.rm=TRUE)
  mavbm_df=mdf(mavbm, coln=stnames)
  write.csv(mavbm_df, file="output/files/bymonth_monthlymean.csv", na = "NA")

#Time Series by months i.e. Jan 1982, 1983, 1984,... Feb 1982, ... 
  ## output is a list of station summaries, each containing 12 lists of monthly TS ##
  ## of daily mean values 
  bymonth_ts=lapply(m_ts, ts.bymonth)
### END AGGREGATION BY MONTH ###

#### SEASONAL AGGREGATION ####
#Daily data by Season
  #Rainy Season
  rs_ts<- lapply(d_ts, mextract, c(1,11:12))
  rs_df=mdf(rs_ts, coln=stnames)
  write.csv(rs_df, file="output/seasonality/daily_rainseason.csv",  na = "NA")

  #Dry Season
  ds_ts <- lapply(d_ts, mextract, c(6:8))
  #str(ds_ts)
  ds_df=mdf(ds_ts, coln=stnames)
  write.csv(rs_df, file="output/seasonality/daily_dryseason.csv", na = "NA")

#Monthly data by Season
  #RS
  #means
  mrs_ts<- lapply(rs_ts, daily2monthly, mean, na.rm=FALSE)
  mrs_df=mdf(mrs_ts, coln=stnames)
  write.csv(mrs_df, file="output/seasonality/monthly_rainseasonmean.csv", na = "NA")
  #sums
  mrss_ts<- lapply(rs_ts, daily2monthly, sum, na.rm=FALSE)
  mrss_df=mdf(mrs_ts, coln=stnames)
  write.csv(mrss_df, file="output/seasonality/monthly_rainseasonsums.csv",  na = "NA")

  #DS
  #means
  mds_ts<- lapply(ds_ts, daily2monthly, mean, na.rm=FALSE)
  mds_df=mdf(mds_ts, coln=stnames)
  write.csv(mds_df, file="output/seasonality/monthly_dryseasonmean.csv",  na = "NA")
  #sums
  mdss_ts<- lapply(ds_ts, daily2monthly, sum, na.rm=FALSE)
  mdss_df=mdf(mdss_ts, coln=stnames)
  write.csv(mdss_df, file="output/seasonality/monthly_dryseasonsums.csv",  na = "NA")

# Aggregate daily to whole season sum/mean by year
  # i.e. mean of rainseason 1982, 1983, .. etc.
  #means
  rsav_ts=lapply(d_ts, daily2season, season="RS", mean, na.rm=FALSE)
  rsav_df=mdf(rsav_ts, coln=stnames)
  write.csv(mdss_df, file="output/seasonality/byyear_rainseasonmeans.csv",  na = "NA")
  #sums
  rss_ts=lapply(d_ts, daily2season, season="RS", sum, na.rm=FALSE)
  rss_df=mdf(rss_ts, coln=stnames)
  write.csv(rss_df, file="output/seasonality/byyear_rainseasonsum.csv",  na = "NA")

  #means
  dsav_ts=lapply(d_ts, daily2season, season="DS", mean, na.rm=FALSE)
  dsav_df=mdf(dsav_ts, coln=stnames)
  write.csv(mdss_df, file="output/seasonality/byyear_dryseasonmeans.csv",  na = "NA")
  #sums
  dss_ts=lapply(d_ts, daily2season, season="DS", sum, na.rm=FALSE)
  dss_df=mdf(dsav_ts, coln=stnames)
  write.csv(mdss_df, file="output/seasonality/byyear_dryseasonsums.csv",  na = "NA")
### END SEASONAL AGGREGATION ###

#### CUMULATIVE DAILY SUMS ####
  cumlist=lapply(d_ts, cumul) #make sums 
  #convert to one connected TS per station
  cumsums=lapply(cumlist, unlist)
  #make times series and convert that to df
  cumsums_ts=lapply(cumsums, function(x) zoo(x, order.by=time(d_ts[[1]])))
  cumsums_df=mdf(cumsums_ts)
  write.csv(cumsums_df, file="output/files/cumulative_funct.csv",  na = "NA") 
### END CUMULATIVE SUMS ###

#### SOI AGGREGATION ####
  ysoi_ts=monthly2annual(soi_ts, mean)
  write.csv(ysoi_ts, file="output/files/soi_yearlymean.csv",  na = "NA")                                                         
### END SOI AGGREGATION ###

########## END aggregate.R #############
