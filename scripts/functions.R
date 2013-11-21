###### PRECIPITATION ANALYSIS: COMPARISON OF GROUND DATA ######

## functions.R contains own functions for the analysis ##

# Coerce zoo time series to data frames function: mdf (make data frame)#
## x: list of zoo time series objects
mdf=function(x){
  dfr=do.call(cbind, as.data.frame(x))		# converison
  row.names(dfr)=as.character(index(x[[1]])) 	# only naming
  colnames(dfr)=stnames				# only naming
  return(dfr)
}

# Make times series for each month i.e. Jan 1970, Jan 1971...
ts.bymonth=function(x){
  bymonth=0
  for (i in 1:12){
    bymonth[i]=list(x[as.numeric(format.Date(time(x), "%m")) %in% c(i)])
  }
  return(bymonth)
}

# own version of correlograms: corgr. creates *.png files in output/plots/ # 
## x: should be a data matrix (as in the normal corrgram() function)
## type: is only for naming e.g. daily, monthly 
corgr=function(x, type){
  require(corrgram)
  name=paste("output/plots/correlograms/",type,"_corgr.png", sep ="")	# filename
  png(filename=name, width=800, height=800, units="px")		# open *.png write
  corrgram(x, lower.panel=panel.pie, upper.panel=panel.pts, main=paste("Correlation between", type, "rainfall amounts"))
  dev.off()							# close write
}
#

# Make  time series plots for each station
tsplot.pst=function(x, type) {
  if (type=="yearly") ptype="b" else ptype="l"
  for (i in 1:length(x)) {
    name=paste("output/plots/time_series/",type,"_ts_",stnames[i],".png", sep="")
    png(filename=name, width=900, height=500, units="px")
    plot(x[[i]], type=ptype, lty=1, lwd=2, col=hexcolors[i], ylab="rainfall in mm/day", main=paste("Time series of", type, "rainfall amounts for", stnames[i]), xlab=substr(type,1, (nchar(type)-2))) 
    dev.off()
  }
}
#
# TS plot for time series by month
tsplot.bymonth.pst=function(x) {
   for (i in 1:length(x)) {
          for (j in 1:12){
            mname=format.Date(time(x[[i]][[j]]), "%m")
    name=paste("output/plots/time_series/bymonth/ts_",stnames[i],"_",mname,".png", sep="")
    png(filename=name, width=900, height=500, units="px")
    plot(x[[i]][[j]], type="l", lty=1, lwd=2, col=hexcolors[i], ylab="rainfall in mm", main=paste("Time series of rainfall amounts for", stnames[i],"by month:", mname)) 
    dev.off()
     }
  }
}

#cumulative function: calculates yearly cumulative sums, accepts NA
## x: time series object
## if there are more than 31 NA values in a row all subsequent terms are set to NA
## e.g yearly sum not calculated

cumul=function(x){
  cum=0
  nacount=0
  timestep=format.Date(time(x),"%m%d")
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
#

#### MOVING AVERAGES PLOT####

#by month
test=0
inp=0
par(mfrow=c(4,3))
for (i in 1:12){
  inp=na.fill(bymonth_ts[[2]][[i]], "extend")
  test=rollmean(inp, 5, fill = "extend", align = "center")
  plot(bymonth_ts[[2]][[i]], col="red")
  lines(test, col="blue")
}


###### END #############