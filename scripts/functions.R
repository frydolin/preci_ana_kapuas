###### PRECIPITATION ANALYSIS: COMPARISON OF GROUND DATA ######

## functions.R contains self written functions for the analysis ##

#### mdf ####
# Coerce zoo time series to data frames: mdf (make data frame)#
## x: list of zoo time series objects
## stnames has to be globally defined if you want nice colnames !!
mdf=function(x){
  dfr=do.call(cbind, as.data.frame(x))		# converison
  row.names(dfr)=as.character(time(x[[1]])) 	# only naming: timestep names
  if(exists("stnames")) colnames(dfr)=stnames	else 	colnames(dfr)=c(1:ncol(dfr)) # only naming: stationnames
  return(dfr)
}
###

####mextract ####
#extract values of certain month 
mextract=function(x, what){
  month=as.numeric(format.Date(time(x), "%m"))
  index <- which(month %in% what)
  return(x[index])
}
###

#### ts.bymonth ####
# Make times series for each month i.e. Jan 1970, Jan 1971...
## x: zoo time series object
## output: a list of 12 (Jan:Dec) zoo objects, each object is one ts by month 
ts.bymonth=function(x){
  bymonth=list()
  for (i in 1:12){
    bymonth[i]=list(x[as.numeric(format.Date(time(x), "%m")) %in% c(i)])
  }
  return(bymonth)
}
###

#### cumul ####
# cumulative function: calculates yearly cumulative sums, accepts NA values
## x: time series object
## if there are more than 31 NA values in a row all subsequent terms are set to NA
## e.g yearly sum not calculated then
cumul=function(x){
  timestep=format.Date(time(x),"%m%d")
  nacount=0   #initiallize variable
  cum=0       #initiallize variable
  cum[1]=x[1]
  for (i in 2:length(x)){
    cum[i]=NA
    if (timestep[i]=="0101") {cum[i]=sum(0,x[i],na.rm=TRUE)
                              nacount=0
                              next}
    if (is.na(x[i])) {nacount=nacount+1}
    else {
      if (nacount==0) {cum[i]=sum(cum[i-1],x[i])
                       nacount=0
                       next}
      if (nacount>0&&nacount<31){cum[i]=sum(cum[i-(nacount+1)],x[i])
                                 nacount=0
                                 next}
      if (nacount>31){cum[i]=NA}
    }    
  }
  return(cum)
}
###

#### daily 2 season ####
# calculates seasonal sums/averages from daily zoo TS
# RS defined as Jan, Nov, Dec. DS defined as Jun, Jul, Aug
daily2season=function(x, season, FUN, na.rm){
  
  if ( !is.zoo(x) ) stop("Invalid argument: 'class(x)' must be zoo")
  if (!(season %in% c("RS","DS"))) stop("season must be either RS or DS")
  
  if (season=="RS"){
    s                 <- mextract(x, c(1,11,12))
    # Moving forward all the Nov and Dec values, in order that
    # Nov and Dec of 1991 be used together with Jan/92 and Feb/92,
    # instead of with Jan/91 and Feb/91
    syears            <- as.numeric(format(time(s), "%Y" ))
    dec.index         <- which(format(time(s), "%m") == c(11,12))
   dec.years         <- syears[dec.index]
   dec.years         <- dec.years + 1
   syears[dec.index] <- dec.years
   
   s.a <- aggregate(s, by= syears, FUN=FUN, na.rm= na.rm)
   
   # Removing the last RS, because it is outside of the analysis period
   s.a <- s.a[1:(length(s.a)-1)]
  }
  
  else{
    s     <- mextract(x, c(6,7,8))
    s.a   <- aggregate(s, by=format(time(s), "%Y"), FUN=FUN, na.rm= na.rm )  
  }
  
  # Replacing the NaNs by 'NA.
  # mean(NA:NA, na.rm=TRUE) == NaN
  nan.index <- which(is.nan(s.a))
  if (length(nan.index) > 0) s.a[nan.index] <- NA 
  
  time(s.a) <- as.Date(paste(time(s.a), "-01-01", sep=""))
  
  return(s.a)
  
}
###

#### corgr ####
# own version of correlograms made by corrgram
# corgr creates *.png files in output/plots/correlograms/
## make sure directory exists!
## x: should be a data matrix (as in the normal corrgram() function)
## type: is only for naming e.g. daily, monthly 
corgr=function(x, type){
  require(corrgram)
  name=paste("output/plots/correlograms/",type,"_corgr.png", sep ="")	# filename
  png(filename=name, width=800, height=800, units="px")		# open *.png write
  corrgram(x, order="PCA", lower.panel=panel.pts, upper.panel=panel.conf, diag.panel=panel.density, main=paste("Correlation between", type, "rainfall amounts"))
  dev.off()							# close write
}
###

#### tsplot.pst ####
# Make  time series plots for each station (time series plot per station)
# creates *.png files in output/plots/time_series/
## make sure directory exists!
## x: zoo time series object
## type: is for naming e.g. daily, monthly. EXCEPTION: "yearly" also changes plot type to "b"!
## colors need to be defined in hexcolors vector.
tsplot.pst=function(x, type) {
  # check if it is yearly ts
  if (type=="yearly") ptype="b" else ptype="l"
  # make graphs
  for (i in 1:length(x)) {
    name=paste("output/plots/time_series/",type,"_ts_",stnames[i],".png", sep="")
    png(filename=name, width=900, height=500, units="px")
    plot(x[[i]], type=ptype, lty=1, lwd=2, col=hexcolors[i], ylab="rainfall in mm/day", main=paste("Time series of", type, "rainfall amounts for", stnames[i]), xlab=substr(type,1, (nchar(type)-2))) 
    dev.off()
  }
}
###

###### END #############
