###### PRECIPITATION ANALYSIS: COMPARISON OF GROUND DATA ######

## functions.R contains self written functions for the analysis ##

#### mdf ####
# Coerce zoo time series to data frames: mdf (make data frame)#
## x: list of zoo time series objects
## coln: names of column headers, default ist stnames
mdf=function(x, coln=stnames){
  dfr=do.call(cbind, as.data.frame(x))		# converison
  row.names(dfr)=as.character(time(x[[1]])) 	# only naming: timestep names
  colnames(dfr)=coln	#only naming column names
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
# cumulative function: calculates yearly cumulative sums
## x: time series object
cumul=function(x){
  ycumsum=list()
  for (i in 1982:2012){
    index=i-1981
    yts=extract(x, trgt=i)
    ycumsum[[index]]=cumsum(yts)
  }
  return(ycumsum)
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

#### make.smry ####
# makes smry summaries and converts them into one comprehensive data frame
## x list of zoo objects 
## assumed they have a common index
## objnames= names of summary objects
make.smry=function(x, objnames=stnames){
  smry.list=lapply(x, smry)
  smry.list[[1]]
  dfr=do.call(cbind, (smry.list))  	# converison
  row.names(dfr)=row.names(smry.list[[1]])
  colselector=c(1,2,seq(4, ncol(dfr), 2)) #remove index columns except first one
  rowselector=c(-10,-11)  #remove skewness and kurtosis entries
  dfr.sub=dfr[rowselector,colselector]  #subset
  colnames(dfr.sub)=c("TIME INDEX",objnames)	#renaming columns
  return(dfr.sub)
}

#### corgr ####
# own version of correlograms made by corrgram
# corgr creates *.png files in fpath
## make sure directory exists!
## x: should be a data matrix (as in the normal corrgram() function)
## type: is only for naming e.g. daily, monthly 
corgr=function(x, type, fpath){
  require(corrgram)
  name=paste(fpath,"/",type,"_corgr.png", sep ="")	# filename
  png(filename=name, width=800, height=800, units="px")		# open *.png write
  corrgram(x, lower.panel=panel.pie, upper.panel=panel.conf, 
           diag.panel=panel.density, 
           main=paste("Correlation between", type, "rainfall amounts"))
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
## fpath: file path, default is fpath
tsplot.pst=function(x, type, fpath) {
  # make directory
  npath=paste(fpath,"/",type, sep="")
  dir.create(npath)
  # check if it is yearly ts
  if(type=="yearly") ptype="b" else ptype="l"
  # make graphs
  for (i in 1:length(x)) {
    name=paste(npath,"/",type,"_ts_",stnames[i],".png", sep="")
    png(filename=name, width=900, height=500, units="px")
    plot(x[[i]], type=ptype, lty=1, lwd=2, col=hexcolors[i], ylab="rainfall in mm/day", main=paste("Time series of", type, "rainfall amounts for", stnames[i]), xlab=substr(type,1, (nchar(type)-2))) 
    dev.off()
  }
}
###
#### Scatterplot Matrix ####
  ### panel.2lines function ###
  # creates lines as input for a scatterplotmatrix
  # lm regression
  # 0,1 abline
  panel.2lines <- function(x,y,...) {
    points(x,y)
    abline(0,1,col="red")
    abline(lm(y~x),col="blue")
  }
  ###
### scatterMatrix###
## x: should be a data matrix (as in the normal corrgram() function)
## type: is only for naming e.g. daily, monthly 
scatterMatrix=function(x, xylim, type, fpath){
  name=paste(fpath,"/",type,"_scatter.png", sep ="")  # filename
  png(filename=name, width=2000, height=2000, units="px")		# open *.png write
  pairs(x, upper.panel=NULL, lower.panel=panel.2lines, 
        xlim=xylim, ylim=xylim,
        main=paste("Correlation between", type, "rainfall amounts"))
  dev.off()							# close write
}
###

###### END #############
