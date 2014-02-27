###### SPATIO-TEMPORAL RAINFALL PATTERNS IN KAPUAS BASIN ######
### Analysis and comparison of station data ###

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
#extract values of certain month(s) 
#x: zoo time series object
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
# x: zoo TS
daily2season=function(x, season, FUN, na.rm){
  # Check for formats
  require("zoo")
  if ( !is.zoo(x) ) stop("Invalid argument: 'class(x)' must be zoo")
  if (!(season %in% c("RS","DS"))) stop("season must be either RS or DS")
  
  # NA threshold
    limit=0
  if (na.rm=="threshold"){
    na.rm=TRUE
    limit=6
      }
  
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
   
   # NA threshold
   if (limit>0){
    #time series of NA values in the original
    #coded as 1, 0
    na.bin=ifelse(is.na(s), 1,0)
   #aggregation of this series, gives NA counts per ag.fac timestep
   na.counts <- aggregate(na.bin, by=list(syears), sum) 
   #index of the too high NA counts
   na.index=which(na.counts$x>limit)
   #replace values with too many NA in the input
   s.a[na.index]=NA
   }
   # Removing first and last RS, because they are outside of the analysis period
   s.a <- s.a[2:(length(s.a)-1)]
  }
  
  else{
    s     <- mextract(x, c(6,7,8))
    s.a   <- aggregate(s, by=format(time(s), "%Y"), FUN=FUN, na.rm= na.rm )
    # NA threshold
    if (limit>0){
      #time series of NA values in the original
      #coded as 1, 0
      na.bin=ifelse(is.na(s), 1,0)
      #aggregation of this series, gives NA counts per ag.fac timestep
      na.counts <- aggregate(na.bin, by=list(format(time(s), "%Y")), sum) 
      #index of the too high NA counts
      na.index=which(na.counts$x>limit)
      #replace values with too many NA in the input
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
#### NA only set after threshold is reached ####
# x: data record to be corrected, should be a list of zoo objects
# orig: source data for x
# type: factor variable for aggregation
# limit: threshold of NA values in orig data at which NAs are introduced into x
na.cor=function(x, orig, type, limit){
        for (i in 1: length(x)){
        # create aggregation factor ist)
          ag.fac=list(cut.Date(time(orig[[i]]), type))
         
        #time series of NA values in the original
        #coded as 1, 0
        orig.na.bin=ifelse(is.na(orig[[i]]), 1,0)
        #aggregation of this series, gives NA counts per ag.fac timestep
        na.counts <- aggregate(orig.na.bin, by=ag.fac, sum) 
        #index of the too high NA counts
        na.index=which(na.counts$x>limit)
      #replace values with too many NA in the input
      x[[i]][na.index]=NA
    }
return(x)
}
 
###
####  true.na.stats ####
# True NA stats, not influenced by differnet record length
# returns NA in %
# also returns the record_length
# x is a zoo object

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
# makes smry summaries and converts them into one comprehensive data frame
## x list of zoo objects 
## assumed they have a common index
## objnames= names of summary objects
make.smry=function(x, objnames=stnames){
  require("hydroTSM")
  smry.list=lapply(x, smry, digits=3)
  dfr=do.call(cbind, (smry.list))  	# converison
  row.names(dfr)=row.names(smry.list[[1]])
  colselector=c(seq(2, ncol(dfr), 2)) #remove every other column (time index)
  rowselector=c(-2,-5,-9,-12,-13)  #remove entries not needed
  dfr.sub=dfr[rowselector,colselector]  #subset
  tdfr.sub=t(dfr.sub)
  #reorder
  dataframe=tdfr.sub[,c(1,4,2,5,3,6,7,8)]
  #get additional statistics, replace NA counst
  add.stats=sapply(x, add.na.stats) 
  add.stats=as.data.frame(add.stats, row.names=c("n", "NA.percentage",
                                                 "first.record",  "last.record" ))
  add.stats=t(add.stats)
  #Put together
  dataframe=as.data.frame(cbind(dataframe, add.stats))
  
  row.names(dataframe)=stnames
  return(dataframe)
}
###
#### corgr ####
# own version of correlograms made by corrgram
# corgr creates *.svg files in fpath
## make sure directory exists!
## x: should be a data matrix (as in the normal corrgram() function)
## type: is only for naming e.g. daily, monthly 
corgr=function(x, type, fpath){
  require("corrgram")
  name=paste(fpath,"/",type,"_corgr.png", sep ="")	# filename
  png(filename=name, pointsize = 11, width=16, height=16, units="cm", res=300)	# open *.svg write
  corrgram(x, lower.panel=panel.pie, upper.panel=panel.conf, diag.panel=panel.density, main="", oma=c(0,0,0,0))
  dev.off()							# close write
}
###

#### Scatterplot Matrix ####
  ### panel.2lines function ###
  # creates lines as input for a scatterplotmatrix
  # lm regression
  # 0,1 abline
  panel.2lines <- function(x,y,...) {
    points(y~x, cex=0.7)
    abline(0,1,col="red")
    abline(lm(y~x),col="darkblue")
  }
  ###
### scatterMatrix###
## x: should be a data matrix (as in the normal corrgram() function)
## type: is only for naming e.g. daily, monthly 
scatterMatrix=function(x, xylim, type, fpath){
  name=paste(fpath,"/",type,"_scatter.png", sep ="")  # filename
  png(filename=name, pointsize = 11, width=16, height=16, units="cm", res=300) # open *.png write
  pairs(x, upper.panel=NULL, lower.panel=panel.2lines, xlim=xylim, ylim=xylim, 
        las=1, gap=0.3, oma=c(2.5,2.5,0,0), cex.labels=1.5, family="Lato"
    )
  dev.off()	# close write
}
###

###### END #############
