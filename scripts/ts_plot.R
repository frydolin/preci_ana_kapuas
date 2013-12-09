###### PRECIPITATION ANALYSIS: COMPARISON OF GROUND DATA ######

## ts_plot.R plots summaries to output files ##

#### SET UP ####

## set up time locale to get english names 
  Sys.setlocale("LC_TIME", "en_US.UTF-8") 

## load functions such as mdf,corgr,.. . As defined in the file.
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

#### Simple Time Series Plots for each station ####
  dir.create("output/plots/time_series")

  tsplot.pst(d_ts, type="daily")
  tsplot.pst(w_ts, type="weekly")
  tsplot.pst(m_ts, type="monthly")
  tsplot.pst(y_ts, type="yearly")

  graphics.off() #Completely shuts down the printing to file
### END TS per Station ###

#### BY MONTH time series with linear trendline ####
  require("zoo")
  dir.create("output/plots/time_series/bymonth") # new directory

#1. Per station: comparison of month within a station
# Creates a plot matrix with all "by month" time series (Jan 1982, Jan 1983, ..)
# for each station
for (i in 1:length(bymonth_ts)) { #loop trough station
  name=paste("output/plots/time_series/bymonth/ts_",stnames[i],".png", sep="")
  png(filename=name, width=2000, height=1200, units="px")
  par(mfrow=c(4,3))
  for (j in 1:12){ #loop trough month
    mname=as.character(format.Date(time(bymonth_ts[[i]][[j]][1]), "%B"))
    title=paste("TS of rainfall sum for",stnames[i],"and month:",mname)
    plot(bymonth_ts[[i]][[j]], type="b", lty=1, lwd=2, col=hexcolors[i], ylab="rainfall in mm", main=title)
    abline(lm(bymonth_ts[[i]][[j]]~time(bymonth_ts[[i]][[j]]))) #trendline
  }
  dev.off()
}
# 2. Per month: comparison of stations for every month
# Creates a plot matrix for all stations of a particular month 
# of the "by month" time series (Jan 1982, Jan 1983, ..)
for (j in 1:12){  # loop through month
  mname=as.character(format.Date(time(bymonth_ts[[1]][[j]][1]), "%B"))
  name=paste("output/plots/time_series/bymonth/ts_",mname,".png", sep="")
  png(filename=name, width=2000, height=1200, units="px")
  par(mfrow=c(4,3))
  for (i in 1:length(bymonth_ts)) {    #loop trough station
    title=paste("TS of rain sum for",stnames[i],"and month:",mname)
    plot(bymonth_ts[[i]][[j]], type="b", lty=1, lwd=2, col=hexcolors[i], ylab="rainfall in mm", main=title)
    abline(lm(bymonth_ts[[i]][[j]]~time(bymonth_ts[[i]][[j]]))) #trendline
  }
  dev.off()
}
### END BY MONTH TS ###

#### ALL STATION TIME SERIES IN ONE PLOT ####
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

### Monthly Averages ###
  name="output/plots/dav_by_month.png"
  png(filename=name, width=1000, height=700, units="px")	
  matplot(davbm_df, type = c("b"),pch=1, lty=c(1), lwd=2, col = hexcolors, xaxt = "n", ylab="rainfall in mm/day", main="Daily Average Rain per Month", xlab="Month")
  axis(1,1:12,labels=row.names(davbm_df))
  legend(x="bottomright", legend=stnames, col=hexcolors, lwd=3, cex=0.8)
  dev.off()
  #boxplot version
  name="output/plots/dav_by_month_boxplot.png"
  png(filename=name, width=1000, height=700, units="px")
  boxplot(t(davbm_df), outline=TRUE, main="Station average of daily average Rain per Month", xlab="Month", ylab="mm/day")
  abline(mean(t(davbm_df), na.rm=TRUE),0, lwd="2", col="blue")
  dev.off()
### END MONTHLY AVERAGES ###


### END ALL STATION TS ### 
  
# #### Cumulative Sums for each station####
# dir.create("output/plots/cumulative")
# 
#   for (i in 1:length(cumfun_ts)) {
#     name=paste("output/plots/cumulative/cumfun_",stnames[i],".png", sep="")
#     png(filename=name, width=800, height=500, units="px")
#     plot(cumfun_ts[[i]], type="l",lty=1, lwd=2, col=hexcolors[i], ylab="rainfall in mm", main=paste("Cumulative rainfall amounts for", stnames[i]), xlab="date") 
#     dev.off()
#   }
# 
# ### END Cumulative Sums ###

#### COMPARISON WITH ENSO ####
  dev.off()
  plot.new()
  par(mar=c(5, 4, 4, 6) + 0.1)
## Plot SOI plot and put axis scale on right
  plot(soi_ts, xlab="", ylab="", main="PTK11 onthly comparison with equatorial SOI", 
       ylim=c(-3.5,3.5), xaxt = "n", yaxt = "n", type="l", col="red")
  polygon(c(min(index(soi_ts)), index(soi_ts), max(index(soi_ts))), 
          c( 0, soi_ts, 0), density=50, col="red") 
  abline(0,0, col="red")
  axis(4, ylim=c(-3,3), col="red",col.axis="red",las=1)
  mtext("SOI INDEX",side=4,col="red",line=3)
## Plot rainfall data and draw its axis
  par(new=TRUE)
  ymid=mean(m_ts[[1]], na.rm=TRUE)
  plot(m_ts[[1]], ylim=c(ymid-12,ymid+12), 
       xaxt = "n", yaxt = "n", xlab="", ylab="", 
       type="l", col="black")
  axis(2, ylim=c(ymid-12,ymid+12),col="black",las=1)  ## las=1 makes horizontal labels
  mtext("raifall [mm/day]",side=2,line=2.5)
## Draw the time axis
axis(1,at=time(y_ts[[1]]), labels=format.Date(time(y_ts[[1]]), "%Y"))
box()
### END ENSO ###

#### shut down ####
rm(name)
graphics.off() #Completely shuts down the printing to file
### END SHUT DOWN ###

########## END OF ts_plot.R #############
