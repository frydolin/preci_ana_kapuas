####### PRECIPITATION ANALYSIS: COMPARISON OF GROUND DATA #######

## aggregate.R creates all necessary summaries and variables to be analysed
## and outputs summary tables 
## see README for variable naming convention 

## aggregation with mean i.e. output is average daily rainfall per week, month, year, ... ##
## and also sums ##


#### SET UP ####
  source("scripts/setup.R")
  ## create output directory ##
  fpath="output/aggregation"
  dir.create(fpath)
### END SET UP ###

#### TIME SERIES AGGREGATION ####
## na.rm usually FALSE
## na.rm=TRUE for monthly and yearly means, 
## but records with too many missing values in the source data are set to NA

#Daily
  #d_ts           #make time series
  #str(d_ts)      #check structure
  d_df=mdf(d_ts, coln=stnames)  #make data frame for better plotting
  write.csv(d_df, file=paste(fpath,"/daily_data.csv", sep=""), na = "NA")

#Weekly
  #mean
  w_ts <- lapply(d_ts, function(x) aggregate(x, by=as.Date(cut.Date(index(x), "7 day")), mean, na.rm=FALSE))
  #str(w_ts)
  w_df=mdf(w_ts, coln=stnames)
  write.csv(w_df, file=paste(fpath,"/weekly_means.csv", sep=""), na = "NA")
  #sum  
  ws_ts <- lapply(d_ts, function(x) aggregate(x, by=as.Date(cut.Date(index(x), "7 day")), sum, na.rm=FALSE))
  ws_df=mdf(w_ts, coln=stnames)
  write.csv(ws_df, file=paste(fpath,"/weekly_sums.csv", sep=""), na = "NA")

#Monthly
  #means
  m_ts <- lapply(d_ts, daily2monthly, mean, na.rm=TRUE)
  # reenter NA for months with too many NA in source data, limit= 3days/per month
  type=list(cut.Date(time(d_ts[[1]]), "months"))
  m_ts=na.cor(m_ts, orig=d_ts, type=type, limit=3)
#   check efficiancy
  sapply(m_ts, function(x) sum(is.na(x)))-
  sapply(m_ts.f, function(x) sum(is.na(x)))
stnames
  m_df=mdf(m_ts, coln=stnames)
  write.csv(m_df, file=paste(fpath,"/monthly_means.csv", sep=""), na = "NA")
  #sums
  ms_ts <- lapply(d_ts, daily2monthly, sum, na.rm=FALSE)
  ms_df=mdf(m_ts, coln=stnames)
  write.csv(ms_df, file=paste(fpath,"/monthly_sums.csv", sep=""), na = "NA")

#Yearly
  y_ts <- lapply(d_ts, daily2annual, mean, na.rm=TRUE)
  # reenter NA for years with too many NA in source data, limit=20 days/year
  type<- list(cut.Date(time(d_ts[[1]]), "years"))
  y_ts.c=na.cor(y_ts, orig=d_ts, type=type, limit=24)
  #   check efficiancy
# sapply(y_ts.f, function(x) sum(is.na(x)))-
#     (sapply(y_ts, function(x) sum(is.na(x))))
# sapply(y_ts.f, function(x) sum(is.na(x)))-
#     sapply(y_ts.c, function(x) sum(is.na(x)))
#     
# stnames
  y_df=mdf(y_ts, coln=stnames)
  write.csv(y_df, file=paste(fpath,"/yearly_means.csv", sep=""), na = "NA")
  #sums
  ys_ts <- lapply(d_ts, daily2annual, sum, na.rm=FALSE)
  ys_df=mdf(ys_ts, coln=stnames)
  write.csv(ys_df, file=paste(fpath,"/yearly_sums.csv", sep=""), na = "NA")

### END TS AGGREGATION ###

#### AGGREGATION BY MONTH ####
#Long term daily average by Month 
#i.e. daily average in all Januries, Febs, etc.
  davbm <- lapply(d_ts, monthlyfunction, mean, na.rm=TRUE)
  #str(davbm)
  davbm_df=mdf(davbm, coln=stnames)
  write.csv(davbm_df, file=paste(fpath,"/bymonth_dailymean.csv", sep=""), na = "NA")

#Long term monthly average by Month 
#i.e. monthly average of all Januries, Febs, etc.
# input are monthly sums for all months
  mavbm <- lapply(ms_ts, monthlyfunction, mean, na.rm=TRUE)
  mavbm_df=mdf(mavbm, coln=stnames)
  write.csv(mavbm_df, file=paste(fpath,"/bymonth_monthlymean.csv", sep=""), na = "NA")

#Time Series by months i.e. Jan 1982, 1983, 1984,... Feb 1982, ... 
  ## output is a list of station summaries, each containing 12 lists of monthly TS ##
  ## of daily mean values 
  bymonth_ts=lapply(m_ts, ts.bymonth)
  #convert to list of dataframes, this format is needed as boxplot input
  bymonth_df_list=lapply(bymonth_ts, mdf, coln=format.Date(time(m_ts[[1]][1:12]), "%b"))
### END AGGREGATION BY MONTH ###

#### SEASONAL AGGREGATION ####
#Daily data by Season
  #Rainy Season
  rs_ts<- lapply(d_ts, mextract, c(1,11:12))
  rs_df=mdf(rs_ts, coln=stnames)
  write.csv(rs_df, file=paste(fpath,"/daily_rainseason.csv", sep=""),  na = "NA")

  #Dry Season
  ds_ts <- lapply(d_ts, mextract, c(6:8))
  #str(ds_ts)
  ds_df=mdf(ds_ts, coln=stnames)
  write.csv(rs_df, file=paste(fpath,"/daily_dryseason.csv", sep=""), na = "NA")

#Monthly data by Season
  #RS
  #means
  mrs_ts<- lapply(rs_ts, daily2monthly, mean, na.rm=FALSE)
  mrs_df=mdf(mrs_ts, coln=stnames)
  write.csv(mrs_df, file=paste(fpath,"/monthly_rainseasonmean.csv", sep=""), na = "NA")
  #sums
  mrss_ts<- lapply(rs_ts, daily2monthly, sum, na.rm=FALSE)
  mrss_df=mdf(mrs_ts, coln=stnames)
  write.csv(mrss_df, file=paste(fpath,"/monthly_rainseasonsums.csv", sep=""),  na = "NA")

  #DS
  #means
  mds_ts<- lapply(ds_ts, daily2monthly, mean, na.rm=FALSE)
  mds_df=mdf(mds_ts, coln=stnames)
  write.csv(mds_df, file=paste(fpath,"/monthly_dryseasonmean.csv", sep=""),  na = "NA")
  #sums
  mdss_ts<- lapply(ds_ts, daily2monthly, sum, na.rm=FALSE)
  mdss_df=mdf(mdss_ts, coln=stnames)
  write.csv(mdss_df, file=paste(fpath,"/monthly_dryseasonsums.csv", sep=""),  na = "NA")

# Aggregate daily to whole season sum/mean by year
  # i.e. mean of rainseason 1982, 1983, .. etc.
  #means
  rsav_ts=lapply(d_ts, daily2season, season="RS", mean, na.rm=FALSE)
  rsav_df=mdf(rsav_ts, coln=stnames)
  write.csv(mdss_df, file=paste(fpath,"/byyear_rainseasonmeans.csv", sep=""),  na = "NA")
  #sums
  rss_ts=lapply(d_ts, daily2season, season="RS", sum, na.rm=FALSE)
  rss_df=mdf(rss_ts, coln=stnames)
  write.csv(rss_df, file=paste(fpath,"/byyear_rainseasonsum.csv", sep=""),  na = "NA")

  #means
  dsav_ts=lapply(d_ts, daily2season, season="DS", mean, na.rm=FALSE)
  dsav_df=mdf(dsav_ts, coln=stnames)
  write.csv(mdss_df, file=paste(fpath,"/byyear_dryseasonmeans.csv", sep=""),  na = "NA")
  #sums
  dss_ts=lapply(d_ts, daily2season, season="DS", sum, na.rm=FALSE)
  dss_df=mdf(dsav_ts, coln=stnames)
  write.csv(mdss_df, file=paste(fpath,"/byyear_dryseasonsums.csv", sep=""),  na = "NA")
### END SEASONAL AGGREGATION ###

#### CUMULATIVE DAILY SUMS ####
  cumlist=lapply(d_ts, cumul) #make sums 
  #convert to one connected TS per station
  cumsums=lapply(cumlist, unlist)
  #make times series and convert that to df
  cumsums_ts=lapply(cumsums, function(x) zoo(x, order.by=time(d_ts[[1]])))
  rm(cumsums)
  cumsums_df=mdf(cumsums_ts)
  write.csv(cumsums_df, file=paste(fpath,"/cumulative_funct.csv", sep=""),  na = "NA") 
### END CUMULATIVE SUMS ###

#### DENSITIES ####
## Compute densities
  library(logspline)
  #library(ks)
  ddensity<-lapply(d_ts, density, from=0, bw="nrd", na.rm=TRUE)
  mdensity<-lapply(m_ts, logspline, lbound = 0)
  ydensity<-lapply(y_ts, density, from=0, bw="nrd", na.rm=TRUE)
### END DENSITIES ###

#### SOI AGGREGATION ####
  ysoi_ts=monthly2annual(soi_ts, mean)
  write.csv(ysoi_ts, file=paste(fpath,"/soi_yearlymean.csv", sep=""),  na = "NA")                                                         
### END SOI AGGREGATION ###

rm(fpath)

########## END aggregate.R #############
