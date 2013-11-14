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

# own version of correlograms: corgr. creates *.png files in output/plots/ # 
## x: should be a data matrix (as in the normal corrgram() function)
## type: is only for naming e.g. daily, monthly 
corgr=function(x, type){
  require(corrgram)
  name=paste("output/plots/",type,"_corgr.png", sep ="")	# filename
  png(filename=name, width=800, height=800, units="px")		# open *.png write
  corrgram(x, lower.panel=panel.pie, upper.panel=panel.pts, main=paste("Correlation between", type, "rainfall amounts"))
  dev.off()							# close write
}


# Make a time series plots for each station
tsplot.pst=function(x, type) {
  for (i in 1:length(x)) {
  name=paste("output/plots/",type,"_ts_",stnames[i],".png", sep="")
  png(filename=name, width=800, height=500, units="px")
  plot(x[[i]], type="l",lty=1, lwd=2, col=hexcolors[i], ylab="rainfall in mm/day", main=paste("Time series of", type, "rainfall amounts for", stnames[i]), xlab=substr(type,1, (nchar(type)-2))) 
  
  }
}

#

##cumulative function

cumul=function(x){
  cum=rep.int(0,length(x))
  nacount=0
  cum[1]=x[1]
  for (i in 2:length(x)){  
    if (is.na(x[i])) {cum[i]=x[i]; nacount=nacount+1
    } else {
        if (nacount==0) {cum[i]=sum(cum[i-1],x[i])}
        if (nacount>0){cum[i]=sum(cum[i-(nacount+1)],x[i])}
        nacount=0
        }
    
    if (format.Date(time(x),"%m%d")[i]=="0101") {cum[i]=sum(0,x[i],na.rm=TRUE); nacount=0}
  }
  return(cum)
}
