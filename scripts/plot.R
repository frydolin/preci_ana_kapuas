###### PRECIPITATION ANALYSIS: COMPARISON OF GROUND DATA ######

## plot.R plots summaries to output ##

#### SET UP ####
Sys.setlocale("LC_TIME", "en_US.UTF-8") #set up time locale to get english names 

##  load functions such as mdf,corgr,.. . As defined in the file.
source("scripts/functions.R")

## create directory ##
dir.create("output/plots", recursive = FALSE)

## COLOR SCHEME ##
  #currently for 11
  hexcolors=c(
    "#222222",
    "#0EC20E",
    "#A30008",
    "#7F054A",
    "#649305",
    "#6D14A2",
    "#17599E",
    "#057E05",
    "#F31D11",
    "#0B9B7F",
    "#F36A11")

#### Correlograms ####
library(corrgram)
dir.create("output/plots/correlograms")

corgr(d_df, type="daily")
corgr(w_df, type="weekly")
corgr(m_df, type="monthly")
corgr(y_df, type="yearly")

corgr(rs_df, type="rainseason daily")
corgr(ds_df, type="dryseason daily")
corgr(mrs_df, type="rainseason monthly")
corgr(mds_df, type="dryseason monthly")

graphics.off() #Completely shuts down the printing to file

### END CORRGRAMS ###

##### TS for each station #####

dir.create("output/plots/time_series")

  tsplot.pst(d_ts, type="daily")
  tsplot.pst(w_ts, type="weekly")
  tsplot.pst(m_ts, type="monthly")
  tsplot.pst(y_ts, type="yearly")

graphics.off() #Completely shuts down the printing to file

### END TS per Station ###

#### ALL STATION TS ####

### Monthly TS ###
name="output/plots/time_series/monthly_ts.png"
png(filename=name, width=1000, height=700, units="px")
matplot(m_df, type = c("l"),pch=1, lwd=2, lty=c(1), col = hexcolors, xaxt = "n", ylab="rainfall in mm/year", main="Yearly Time Series", xlab="Year")
axis(1,1:372,labels=substr(row.names(m_df),1,7))  
legend(x="bottomleft", legend=stnames, col=hexcolors, lwd=3, cex=0.8)
dev.off()

### YEARLY TS ###
name="output/plots/time_series/yearly_ts.png"
png(filename=name, width=1000, height=700, units="px")
matplot(y_df, type = c("b"),pch=1, lwd=2, lty=c(1), col = hexcolors, xaxt = "n", ylab="rainfall in mm/year", main="Yearly Time Series", xlab="Year")
axis(1,1:31,labels=substr(row.names(y_df),1,4))  
legend(x="bottomright", legend=stnames, col=hexcolors, lwd=3, cex=0.8)
dev.off()

### Comparison PTK11- KPH01 ###
plot(window(m_ts[[1]], start=as.Date('2002-01-01'), end=as.Date('2010-12-31')),  type="l", lty=1, lwd=2, col=hexcolors[1], ylab="rainfall in mm/day", main=paste("Time series of daily rainfall amounts for",stnames[c(1,11)], collapse=" "), xlab="Time") 
lines(window(m_ts[[11]], start=as.Date('2002-01-01'), end=as.Date('2010-12-31')), col=hexcolors[11])

#### END ALL STATION TS ####

#### Box plot for station comparison ####
# year
  boxplot(y_df, outline=FALSE)
  abline(mean(y_df,  na.rm=TRUE),0)
  beeswarm(y_ts, col=hexcolors, add=TRUE)

#monthly per month i.e. Jan
  bplot(m_df[format.Date(as.Date(row.names(m_df)),format="%m")=="01",])
  abline(mean(m_df, na.rm=TRUE),0)

#### END BOX PLOTS ####


##### Monthly Averages #####
  name="output/plots/dav_by_month.png"
  png(filename=name, width=1000, height=700, units="px")	
  matplot(davbm_df, type = c("b"),pch=1, lty=c(1), lwd=2, col = hexcolors, xaxt = "n", ylab="rainfall in mm/day", main="Daily Average Rain per Month", xlab="Year")
  axis(1,1:12,labels=row.names(davbm_df))
  legend(x="bottomright", legend=stnames, col=hexcolors, lwd=3, cex=0.8)
  dev.off()

### END MONTHLY AVERAGES ###
  
#### Cumulative Sums ####
dir.create("output/plots/cumulative")

  for (i in 1:length(cumfun_ts)) {
    name=paste("output/plots/cumulative/cumfun_",stnames[i],".png", sep="")
    png(filename=name, width=800, height=500, units="px")
    plot(cumfun_ts[[i]], type="l",lty=1, lwd=2, col=hexcolors[i], ylab="rainfall in mm", main=paste("Cumulative rainfall amounts for", stnames[i]), xlab="date") 
    dev.off()
  }

### Comparison of cumulative sums ###
  ## 1. Comparison of SGU 1, 19, 17 since they are spatially close
  #10year 
  name=paste("output/plots/cumulative/10ycumsum_comparison.png", sep="")
  png(filename=name, width=800, height=500, units="px")
  matplot(cumfun_df[6575:10592,c(3,7,8)], type = c("l"), xaxt="n",  pch=1, lwd=2, lty=c(1), col =hexcolors[c(3,7,8)], ylab="rainfall in mm/year", main=paste("Cumulative rainfall amounts for", stnames[c(3,7,8)]), xlab="Year")
  axis(1,at=seq(0, 4015, by=365), labels=c(2000:2011)  )
  legend(x="topright", legend=stnames[c(3,7,8)], col=hexcolors[c(3,7,8)], lwd=3, cex=0.8)
  dev.off()
  #2 year
  name=paste("output/plots/cumulative/2ycumsum_comparison.png", sep="")
  png(filename=name, width=800, height=500, units="px")
  matplot(cumfun_df[7671:8402,c(3,7,8)], type = c("l"), xaxt="n",  pch=1, lwd=2, lty=c(1), col =hexcolors[c(3,7,8)], ylab="rainfall in mm/year", main=paste("Cumulative rainfall amounts for", stnames[c(3,7,8)]), xlab="Year")
  axis(1,at=seq(0, 740, by=365), labels=c(2003:2004)  )
  legend(x="topright", legend=stnames[c(3,7,8)], col=hexcolors[c(3,7,8)], lwd=3, cex=0.8)
  dev.off()
## 2. Comparison of PTK11, SGU01, STG01 KPH01, in order to see west east gradient
  #3 year
  name=paste("output/plots/cumulative/3ycumsum_ew_comparison.png", sep="")
  png(filename=name, width=1000, height=500, units="px")
stnamestring=paste(stnames[c(1,3,9,11)], collapse=" ")
  matplot(cumfun_df[7671:8767,c(1,3,9,11)], type = c("l"), xaxt="n",  pch=1, lwd=2, lty=c(1), col =hexcolors[c(1,3,9,11)], ylab="rainfall in mm/year", main=paste("Cumulative rainfall amounts for",stnames[c(1,3,9,11)], collapse=" "), xlab="Year")
  axis(1,at=seq(0, 1105, by=365), labels=c(2003:2006)  )
  legend(x="topleft", legend=stnames[c(1,3,9,11)], col=hexcolors[c(1,3,9,11)], lwd=3, cex=0.8)
  dev.off()
### END Cumulative Sums ###

#### BY MONTH time series ####
dir.create("output/plots/time_series/bymonth")
for (i in 1:length(bymonth_ts)) {
  for (j in 1:12){
    mname=as.character(format.Date(time(bymonth_ts[[i]][[j]][1]), "%B"))
    title=paste("Time series of rainfall amounts for",stnames[i],"by month:",mname)
    name=paste("output/plots/time_series/bymonth/ts_",stnames[i],"_",j,"_",mname,".png", sep="")
    png(filename=name, width=900, height=500, units="px")
    plot(bymonth_ts[[i]][[j]], type="b", lty=1, lwd=2, col=hexcolors[i], ylab="rainfall in mm", main=title)
    dev.off()
      }
}

#shut down
rm(name)
dev.off()
graphics.off() #Completely shuts down the printing to file

########## END #############
