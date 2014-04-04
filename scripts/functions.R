###### SPATIO-TEMPORAL RAINFALL PATTERNS IN KAPUAS BASIN ######
	### ANALYSIS AND COMPARISON OF GAUGE DATA ###

## functions.R:
## contains often used self written functions
## List of functions in alphabetical order: 
## add.na.stats, corgr, cumul, daily2season, make.smry, mextract, mdf, na.cor, panel.2lines, scatterMatrix, ts.bymonth
## file convenience_function.R contains further more specific functions

#### mdf ####
# Coerce zoo time series to data frame. mdf means "make data frame"
## x: list of zoo time series objects
## coln: names of column headers, default ist stnames
mdf=function(x, coln=stnames){
  dfr=do.call(cbind, as.data.frame(x))		# making the data frame
  row.names(dfr)=as.character(time(x[[1]])) 	# only naming: timestep names
  colnames(dfr)=coln	#only naming column names
  return(dfr)
}
###

#### mextract ####
# Extract values belonging to certain month(s) 
## x: zoo time series object
## what: vector, months to extraxt
mextract=function(x, what){
  require("zoo")
  month=as.numeric(format.Date(time(x), "%m"))
  index <- which(month %in% what)
  return(x[index])
}
###

#### ts.bymonth ####
# Make times series for each month i.e. Jan 1970, Jan 1971...
## x: zoo time series object
## output is a list of 12 (Jan:Dec) zoo objects, each object a time series of one month
ts.bymonth=function(x){
  bymonth=list()
  for (i in 1:12){
    bymonth[i]=list(x[as.numeric(format.Date(time(x), "%m")) %in% c(i)]) # extract values of month i
  }
  return(bymonth)
}
###

#### cumul ####
# Calculate cumulative sums per year
## timeperiod fixed to 1982:2012
## x: zoo time series object
cumul=function(x){
  require("hydroTSM")
  ycumsum=list()
  for (i in 1982:2012){
    index=i-1981
    yts=hydroTSM::extract(x, trgt=i)
    ycumsum[[index]]=cumsum(yts)
  }
  return(ycumsum)
}
###

#### daily2season ####
# Calculate seasonal sums/averages from daily zoo TS
## x: zoo time series object
## season: "RS" or "DS", Rain season (RS) defined as Jan, Nov, Dec. Dry season (DS) defined as Jun, Jul, Aug
## FUN: mean or sum
## na.rm: TRUE/FALSE (usual behavior) or "threshold" (function only accepts certain number of NAs)
daily2season=function(x, season, FUN, na.rm){
	require("zoo")
  # Check input
  if (!is.zoo(x) ) stop("Invalid argument: 'class(x)' must be zoo")
  if (!(season %in% c("RS","DS"))) stop("season must be either RS or DS")
  
  # NA threshold up until which NAs in the input data are accepted
    limit=0
  if (na.rm=="threshold"){
    na.rm=TRUE
    limit=6}
  
  if (season=="RS"){
    	s                 <- mextract(x, c(1,11,12))
    # Moving forward all the Nov and Dec values, in order that e.g. Nov and Dec of 1991 be used together with Jan/92 and Feb/92,instead of with Jan/91 and Feb/91
		  syears            <- as.numeric(format(time(s), "%Y" ))
		  dec.index         <- which(format(time(s), "%m") == c(11,12))
		  dec.years         <- syears[dec.index]
		  dec.years         <- dec.years + 1
		  syears[dec.index] <- dec.years
    # aggregation
			s.a <- aggregate(s, by= syears, FUN=FUN, na.rm= na.rm)
			
		# Reintroduce NAs if there is a NA threshold
			if (limit>0){
    	# time series of NA values in the original coded as 1, 0
				na.bin=ifelse(is.na(s), 1,0)
			# aggregation of this series gives NA counts per timestep (season)
				na.counts <- aggregate(na.bin, by=list(syears), sum) 
			# index of the seasons with  too high NA counts
				na.index=which(na.counts$x>limit)
			# replace values where there are too many NA in the input
				s.a[na.index]=NA
			 }
			 
		# Remove first and last RS, because they are outside of the analysis period
			s.a <- s.a[2:(length(s.a)-1)]
  }
  
  else{ # i.e. not RS, so DS
  	# Extraction and aggregation
    s     <- mextract(x, c(6,7,8))
    s.a   <- aggregate(s, by=format(time(s), "%Y"), FUN=FUN, na.rm= na.rm )
    
    # Reintroduce NAs if there is a NA threshold
			if (limit>0){
    	# time series of NA values in the original coded as 1, 0
				na.bin=ifelse(is.na(s), 1,0)
			# aggregation of this series gives NA counts per timestep (season)
				na.counts <- aggregate(na.bin, by=list(syears), sum) 
			# index of the seasons with  too high NA counts
				na.index=which(na.counts$x>limit)
			# replace values where there are too many NA in the input
				s.a[na.index]=NA
			 }
    }
  # Replacing the NaNs by 'NA.
  # mean(NA:NA, na.rm=TRUE) == NaN
  nan.index <- which(is.nan(s.a))
  if (length(nan.index) > 0) s.a[nan.index] <- NA 
  #rewrite time
  time(s.a) <- as.Date(paste(time(s.a), "-01-01", sep=""))
  
  return(s.a)
}
###

#### na.cor ####
# Set records to NA after threshold is reached (NA correction)
## x: data record to be corrected, list of zoo objects
## orig: source data from which x was computed, list of zoo objects
## type: "month" or "year", or other input cut.Date accepts, used to make aggregation factor
## limit: threshold of NA values in orig data after which NAs should be introduced into x
na.cor=function(x, orig, type, limit){
        for (i in 1: length(x)){ # runs through the list of zoo objects
        # create aggregation factor list
          ag.fac=list(cut.Date(time(orig[[i]]), type))
				#time series of NA values in the original coded as 1, 0
					orig.na.bin=ifelse(is.na(orig[[i]]), 1,0)
        #aggregation of this series, gives NA counts per timestep of the aggregation factor
					na.counts <- aggregate(orig.na.bin, by=ag.fac, sum) 
        #index of the too high NA counts
					na.index=which(na.counts$x>limit)
				#replace values with too many NA in the input
					x[[i]][na.index]=NA
				}
	return(x)
}
###

#### add.na.stats ####
# Additional, "true" NA statistics. Not influenced by the fact that records from 1982-01-01 until the start of data availability are also set to NA in the input data
## x: zoo object
add.na.stats= function(x){
  index.non.NA  <- which(!is.na(x))
  first.non.NA  <- min(index.non.NA)
  last.non.NA   <- max(index.non.NA)
  record.length <- length(x[first.non.NA:last.non.NA])
  true.NA.sum   <- sum(is.na(x[first.non.NA:last.non.NA]))
  NA.percentage <- (true.NA.sum/(record.length))*100
  first.record  <- as.character(time(x[first.non.NA]))
  last.record   <- as.character(time(x[last.non.NA]))
  return(rbind("n"=record.length, "NA.percentage"=NA.percentage, "first.record"=first.record, 
              "last.record"=last.record))
}
###

#### make.smry ####
# Make summaries and convert them into one comprehensive data frame
## x: list of zoo objects, it is assumed they have a common index
## objnames: names of obects to be summarized
make.smry=function(x, objnames=stnames){
  require("hydroTSM")
  smry.list=lapply(x, smry, digits=3) # use hydroTSM::smry as summary function
  dfr=do.call(cbind, (smry.list))  	# make data frame
  row.names(dfr)=row.names(smry.list[[1]])
  colselector=c(seq(2, ncol(dfr), 2)) #remove every other column (time indeces)
  rowselector=c(-2,-5,-9,-12,-13)  #remove entries not needed
  dfr.sub=dfr[rowselector,colselector]  #subset
  tdfr.sub=t(dfr.sub) #transpose
  dataframe=tdfr.sub[,c(1,4,2,5,3,6,7,8)] #reorder
  # get additional statistics, replace NA counts
  add.stats=sapply(x, add.na.stats) 
  add.stats=as.data.frame(add.stats, row.names=c("n", "NA.percentage", "first.record",  "last.record" ))
  add.stats=t(add.stats)
  # Put together
  dataframe=as.data.frame(cbind(dataframe, add.stats))
  row.names(dataframe)=stnames
  
  return(dataframe)
}
###

#### panel.2lines function ####
# xy-plot with regression line and bisecting line, used in a scatterplotmatrix or corrgram panel (see below)
## x: x values
## y: y values
## ...: further arguments passed on
panel.2lines <- function(x,y,...) {
  points(y~x, cex=0.7)
  abline(0,1,col="blue") # bisecting line
  abline(lm(y~x),col="red") # regression with linear model
  box(col = "lightgray")
}
###

#### corgr ####
# Creates Correlograms. Basically a wrapper function for corrgram::corrgram, with own specifications.
# corgr creates *.svg files in fpath, make sure directory exists!
## x: should be data matrix (as in the normal corrgram() function)
## xylim: vector with two components, for xlim and ylim, in order to have same scaling on both axes
## type: is only for naming e.g. daily, monthly 
## fpath: character, output file path
corgr=function(x, xylim, type, fpath){
  require("corrgram")
  name=paste(fpath,"/",type,"_corgr.png", sep ="")	# filename
  png(filename=name, pointsize = 11, width=16, height=16, units="cm", res=300)	# open *.png write
  corrgram(x, lower.panel=panel.pie, upper.panel=panel.2lines, diag.panel=panel.density, xlim=xylim, ylim=xylim, main="", oma=c(0,0,0,0))
  dev.off()	# close write
}
###

#### scatterMatrix ####
# Creates a Scatterplot Matrix. Basically a wrapper for graphics::pairs.
## x: should be a data matrix, its columns are plotted against each other
## xylim: vector with two components, for xlim and ylim, in order to have same scaling on both axes
## type: is only for naming e.g. daily, monthly 
## fpath: character, output file path
scatterMatrix=function(x, xylim, type, fpath){
  name=paste(fpath,"/",type,"_scatter.png", sep ="")  # filename
  png(filename=name, pointsize = 11, width=16, height=16, units="cm", res=300) # open *.png write
  pairs(x, upper.panel=NULL, lower.panel=panel.2lines, xlim=xylim, ylim=xylim, las=1, gap=0.3, oma=c(2.5,2.5,0,0), cex.labels=1.5, family="Lato")
  dev.off()	# close write
}
###

###### END functions.R ######
