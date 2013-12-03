###### PRECIPITATION ANALYSIS: COMPARISON OF GROUND DATA ######

## sp_plot.R plots spatial and comparative summaries to output files ##

#### SET UP ####

## set up time locale to get english names 
Sys.setlocale("LC_TIME", "en_US.UTF-8") 

##  load functions such as mdf,corgr,.. . As defined in the file.
source("scripts/functions.R")

## create plot output directory ##
dir.create("output/plots", recursive = FALSE) 

## COLOR SCHEME for plots##
# currently for 11 stations
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
### END SET UP ###

#### Box plot for station comparison ####
library(beeswarm)
# year
boxplot(y_df, outline=FALSE)
abline(mean(y_df,  na.rm=TRUE),0)
beeswarm(y_ts, col=hexcolors, add=TRUE)

#monthly per month i.e. Jan
bplot(m_df[format.Date(as.Date(row.names(m_df)),format="%m")=="01",])
abline(mean(m_df, na.rm=TRUE),0)
### END BOX PLOTS ###

#### CORRELOGRAMS ####
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

#### Cumulative Sums ####
dir.create("output/plots/cumulative")

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

### Comparison PTK11- KPH01 ###
plot(window(m_ts[[1]], start=as.Date('2002-01-01'), end=as.Date('2010-12-31')),  type="l", lty=1, lwd=2, col=hexcolors[1], ylab="rainfall in mm/day", main=paste("Time series of daily rainfall amounts for",stnames[c(1,11)], collapse=" "), xlab="Time") 
lines(window(m_ts[[11]], start=as.Date('2002-01-01'), end=as.Date('2010-12-31')), col=hexcolors[11])

###  ###


#### shut down ####
rm(name)
dev.off()
graphics.off() #Completely shuts down the printing to file
### END SHUT DOWN ###

########## END OF sp_plot.R #############
