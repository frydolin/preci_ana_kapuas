###### SPATIO-TEMPORAL RAINFALL PATTERNS IN KAPUAS BASIN ######
	### ANALYSIS AND COMPARISON OF GAUGE DATA ###

## aggregate.R creates necessary summaries and variables to be analysed
## and outputs summary tables 
## see README for variable naming convention 

## aggregation with mean i.e. output is average daily rainfall per week, month, year, ... and also with sums

#### SET UP ####
  source("scripts/setup.R")
  
# create output directory
	fpath="output/aggregation"
	dir.create(fpath)
### END SET UP ###

#### CUT OFF UNREALISTICALLY HIGH VALUES ####
# above 250mm
	for(i in (1:length(d_ts))){
		high.ind=which(d_ts[[i]]>250)
		d_ts[[i]][high.ind]=NA
		}
		rm(i, high.ind)
### 

#### TIME SERIES AGGREGATION ####
## aggregation functions mostly from hydroTSM package
## na.rm usually FALSE
## na.rm=TRUE for monthly and yearly means, but records with too many missing values in the source data are set to NA afterwards

#Daily
  #d_ts           # make time series
  #str(d_ts)      # check structure
  d_df=mdf(d_ts, coln=stnames)  # make data frame for easier handling in some applications
  write.csv(d_df, file=paste(fpath,"/daily_data.csv", sep=""), na = "NA")	# write data frame to *.csv

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
  # reenter NA for months with too many NA in source data (limit= 3days/permonth):
  m_ts=na.cor(m_ts, orig=d_ts, type="month", limit=3) #na.cor is the function that reintroduces NAs, see functions.R
  #df
  m_df=mdf(m_ts, coln=stnames)
  write.csv(m_df, file=paste(fpath,"/monthly_means.csv", sep=""), na = "NA")
  #sums
  ms_ts <- lapply(d_ts, daily2monthly, sum, na.rm=TRUE)
  ms_ts=na.cor(ms_ts, orig=d_ts, type="month", limit=3) #function that reintroduces NAs, see functions.R
  ms_df=mdf(ms_ts, coln=stnames)
  write.csv(ms_df, file=paste(fpath,"/monthly_sums.csv", sep=""), na = "NA")

#Yearly
  y_ts <- lapply(d_ts, daily2annual, mean, na.rm=TRUE)
  # reenter NA for years with too many NA in source data, limit=28 days/year:
  y_ts=na.cor(y_ts, orig=d_ts, type="years", limit=28) #function that reintroduces NAs, see functions.R
  y_df=mdf(y_ts, coln=stnames)
  write.csv(y_df, file=paste(fpath,"/yearly_means.csv", sep=""), na = "NA")
  #sums
  ys_ts <- lapply(d_ts, daily2annual, sum, na.rm=TRUE)
  ys_ts=na.cor(ys_ts, orig=d_ts, type="years", limit=28)
  ys_df=mdf(ys_ts, coln=stnames)
  write.csv(ys_df, file=paste(fpath,"/yearly_sums.csv", sep=""), na = "NA")
  
#Number of rainy days in a year
  #if rainfall>1 mm 1 otherwise 0
  d_rainday<-lapply(d_ts, function (x) ifelse(x>1, 1, 0))
  #to yearly
  y_raindays<-lapply(d_rainday, daily2annual,sum, na.rm=TRUE)
  # reenter NA for years with too many NA in source data, limit=28 days/year
  y_raindays=na.cor(y_raindays, orig=d_ts, type="year", limit=28)
  y_raindays_df=mdf(y_raindays)
  rm(d_rainday)
### END TIME SERIES AGGREGATION ###

#### AGGREGATION BY MONTH ####
# Long term daily average by Month 
# i.e. daily average in all Januries, Febs, etc.
# input are daily means for all months
  davbm <- lapply(m_ts, monthlyfunction, mean, na.rm=TRUE)
  davbm_df=mdf(davbm, coln=stnames)
  write.csv(davbm_df, file=paste(fpath,"/bymonth_dailymean.csv", sep=""), na = "NA")

# Long term monthly average by Month 
# i.e. monthly average of all Januries, Febs, etc.
# input are monthly sums for all months
  mavbm <- lapply(ms_ts, monthlyfunction, mean, na.rm=TRUE)
  mavbm_df=mdf(mavbm, coln=stnames)
  write.csv(mavbm_df, file=paste(fpath,"/bymonth_monthlymean.csv", sep=""), na = "NA")

# Time Series by months i.e. Jan 1982, 1983, 1984,... Feb 1982, ... 
  ## output is a list with one entry per station, each of these contains one list per month (so 12) of mean daily rainfall in that month
  bymonth_ts=lapply(m_ts, ts.bymonth)
  
  #convert to list of dataframes; this format is needed as boxplot input
  bymonth_df_list=lapply(bymonth_ts, mdf, coln=format.Date(time(m_ts[[1]][1:12]), "%b"))

	#convert to a special format, needed for beeplot: a list of twelve (month), each entry containing an array of stations and their monthly values for that month
	bymonth_ts_all=list()
	for (i in 1:12){
			bymonth_ts_all[[i]]<-sapply(bymonth_ts, function(x) x[[i]])
	}
### END AGGREGATION BY MONTH ###

#### SEASONAL AGGREGATION ####
#Daily data for the seasons
  #Rainy Season (Nov, Dec, January)
  rs_ts<- lapply(d_ts, mextract, c(1,11:12))
  rs_df=mdf(rs_ts, coln=stnames)
  write.csv(rs_df, file=paste(fpath,"/daily_rainseason.csv", sep=""),  na = "NA")

  #Dry Season (Jun, July, August)
  ds_ts <- lapply(d_ts, mextract, c(6:8))
  ds_df=mdf(ds_ts, coln=stnames)
  write.csv(rs_df, file=paste(fpath,"/daily_dryseason.csv", sep=""), na = "NA")

#Monthly data for the seasons
  #Rainy Season (Nov, Dec, January)
    #means
    mrs_ts<-lapply(m_ts, mextract, c(1,11:12))
    mrs_df=mdf(mrs_ts, coln=stnames)
    write.csv(mrs_df, file=paste(fpath,"/monthly_rainseasonmean.csv", sep=""), na = "NA")
    #sums
    mrss_ts<- lapply(ms_ts, mextract, c(1,11:12))
    mrss_df=mdf(mrs_ts, coln=stnames)
    write.csv(mrss_df, file=paste(fpath,"/monthly_rainseasonsums.csv", sep=""), na = "NA")

  #Dry Season (Jun, July, August)
    #means
    mds_ts<- lapply(m_ts, mextract, c(6:8))
    mds_df=mdf(mds_ts, coln=stnames)
    write.csv(mds_df, file=paste(fpath,"/monthly_dryseasonmean.csv", sep=""),  na = "NA")
    #sums
    mdss_ts<- lapply(ms_ts, mextract, c(6:8))
    mdss_df=mdf(mdss_ts, coln=stnames)
    write.csv(mdss_df, file=paste(fpath,"/monthly_dryseasonsums.csv", sep=""),  na = "NA")

# Means and sums of seasons
	# Aggregate daily to whole season sum/mean by year
  # i.e. mean of rainseason 1982, 1983, .. etc.
  # Nov and Dec values are moved forward, in order that e.g. Nov and Dec of 1991 is used together with Jan/92 instead of with Jan/91 
  # for na.rm="threshold" NAs are introduced if there are more than 6 missing values per season, see function daily2season in functions.R for details

  # Rain season:
    #means
    rsav_ts=lapply(d_ts, daily2season, season="RS", mean, na.rm="threshold") # function daily2season is a self-written function, see functions.R
    rsav_df=mdf(rsav_ts, coln=stnames)
    write.csv(rsav_df, file=paste(fpath,"/byyear_rainseasonmeans.csv", sep=""),  na = "NA")
    #sums
    rss_ts=lapply(d_ts, daily2season, season="RS", sum, na.rm="threshold")
    rss_df=mdf(rss_ts, coln=stnames)
    write.csv(rss_df, file=paste(fpath,"/byyear_rainseasonsum.csv", sep=""),  na = "NA")
  #Dry Season
    #means
    dsav_ts=lapply(d_ts, daily2season, season="DS", mean, na.rm="threshold")
    dsav_df=mdf(dsav_ts, coln=stnames)
    write.csv(dsav_df, file=paste(fpath,"/byyear_dryseasonmeans.csv", sep=""),  na = "NA")
    #sums
    dss_ts=lapply(d_ts, daily2season, season="DS", sum, na.rm="threshold")
    dss_df=mdf(dss_ts, coln=stnames)
    write.csv(dss_df, file=paste(fpath,"/byyear_dryseasonsums.csv", sep=""),  na = "NA")
### END SEASONAL AGGREGATION ###

#### CUMULATIVE SUMS ####
# create output directory
  fpath="output/cumulative"
  dir.create(fpath)
## cumulative sums for each year
  cumlist=lapply(d_ts, cumul) # cumul is a self-written wrapper function for cumsum, see functions.R 
  # convert to one connected time series per station
  cumsums=lapply(cumlist, unlist)
  cumsums_ts=lapply(cumsums, function(x) zoo(x, order.by=time(d_ts[[1]]))) # convert to ts
  cumsums_df=mdf(cumsums_ts) # convert to df
  write.csv(cumsums_df, file=paste(fpath,"/cumulative_sums.csv", sep=""),  na = "NA") 
  rm(cumsums)
  
## cumulative sum of average rain on each day over the whole time period
  # aggregate mean daily rainfall per day
  dpd_ts=lapply(d_ts, aggregate, by=format.Date(time(d_ts[[1]]), "%m-%d"), mean, na.rm=TRUE) #"daily per day" time series
  d.cumsum_ts=lapply(dpd_ts, cumsum) # make cumulative sum
  d.cumsum_df=mdf(d.cumsum_ts)
  rm(dpd_ts)
### END CUMULATIVE SUMS ###

#### KERNEL DENSITIES AND DISTRIBUTION FUNCTIONS ####
## Compute gaussian Kernel density estimates (KDEs)
  ddensity_rd<-lapply(d_ts, function (x) density(x[which(x>=1)], from=0, bw=3.5,  na.rm=TRUE)) # only days with more than 1mm rain
  ddensity_ad<-lapply(d_ts, function (x) density(x, from=0, bw=3.5,  na.rm=TRUE)) # all days
  mdensity<-lapply(m_ts, function (x) density(x, from=0,  bw=1.8,  na.rm=TRUE))
	ydensity<-lapply(y_ts, function (x) density(x, from=0, bw=0.75,  na.rm=TRUE))
	y_rainday.density<-lapply(y_raindays, function (x) density(x, from=0, bw=12,  na.rm=TRUE))

## Empirical Cumulative Distribution Functions (ECDFs) ##
  d.ecdf<-lapply(d_ts, function (x) ecdf(as.ts(x[which(x>=1)]))) # only days with more than 1mm rain
  m.ecdf<-lapply(m_ts, function (x) ecdf(as.ts(x)))
  y.ecdf<-lapply(y_ts,  function (x) ecdf(as.ts(x)))
### END DENSITIES AND DISTRIBUTION FUNCTIONS ###

#### CLEAN UP ####
rm(fpath,i)
###

###### END aggregate.R ######
