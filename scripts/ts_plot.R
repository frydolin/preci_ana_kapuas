###### PRECIPITATION ANALYSIS: COMPARISON OF STATION DATA ######

## ts_plot.R plots summaries to output files ##

#### SET UP ####
## set up time locale to get english names 
  Sys.setlocale("LC_TIME", "en_US.UTF-8") 

## load functions such as mdf,corgr,.. . As defined in the file.
  source("scripts/functions.R")

## create plot output directory ##
  dir.create("output/plots")

## COLOR SCHEME for plots##
  # currently for 13 stations
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
    "#F36A11",
    "#FF00FF",
    "#00FFFF")
### END SET UP ###

#### SIMPLE TS PLOTS FOR EACH STATION ####
  dir.create("output/time_series")

  tsplot.pst(d_ts, type="daily")
  daily_summary=make.smry(d_ts)
  write.csv(daily_summary, file="output/time_series/daily_summary.csv", na = "NA")
  tsplot.pst(w_ts, type="weekly")
  tsplot.pst(m_ts, type="monthly")
  monthly_summary=make.smry(m_ts)
  write.csv(monthly_summary, file="output/time_series/monthly_summary.csv", na = "NA")
  tsplot.pst(y_ts, type="yearly")
  yearly_summary=make.smry(y_ts)
  write.csv(yearly_summary, file="output/time_series/yearly_summary.csv", na = "NA")
### END TS per Station ###

#### HISTOGRAMS ####
library("MASS")
dir.create("output/plots/histogramms") # new directory

# Per station
for (i in 1:length(d_ts))
{
  name=paste("output/plots/histogramms/hist_",stnames[i],".png", sep="")
  png(filename=name, width=1500, height=2500, units="px")
  par(mfrow=c(3,1))
  title=paste("values histogramm for",stnames[i])
  truehist(d_ts[[i]], h=5, xlim=c(0,200), col=hexcolors[i], bty="o", main=paste("Daily",title))
  truehist(m_ts[[i]], h=1, xlim=c(0,30), col=hexcolors[i], bty="o", main=paste("Monthly",title))
  truehist(y_ts[[i]], h=0.5, xlim=c(5,15), col=hexcolors[i], bty="o", main=paste("Yearly",title))
  dev.off()
}
# Per type
  # Daily
  name=paste("output/plots/histogramms/daily_hist.png", sep="")
  png(filename=name, width=1500, height=1200, units="px")
  par(mfrow=c(3,4))
  for (i in 1:12){
    title=paste("values histogramm for",stnames[i])
    truehist(d_ts[[i]], h=5, xlim=c(0,200), prob=FALSE,
             col=hexcolors[i], bty="o", main=paste("Daily",title))
  }
  dev.off()
  #Monthly
  name=paste("output/plots/histogramms/monthly_hist.png", sep="")
  png(filename=name, width=1500, height=1200, units="px")
  par(mfrow=c(3,4))
  for (i in 1:12){
    title=paste("values histogramm for",stnames[i])
    truehist(m_ts[[i]], h=1, xlim=c(0,30), prob=FALSE,
             col=hexcolors[i], bty="o", main=paste("Monthly",title))
  }
  dev.off()
  #Yearly
  name=paste("output/plots/histogramms/yearly_hist.png", sep="")
  png(filename=name, width=1500, height=1200, units="px")
  par(mfrow=c(3,4))
  for (i in 1:12){
    title=paste("values histogramm for",stnames[i])
    truehist(y_ts[[i]], h=0.5, xlim=c(5,15), prob=FALSE,
             col=hexcolors[i], bty="o", main=paste("Yearly",title))
  }
  dev.off()

#### DENSITY ####

# Plot Per station
for (i in 1:length(d_ts))
{
  name=paste("output/plots/histogramms/density_",stnames[i],".png", sep="")
  png(filename=name, width=1000, height=2000, units="px")
  par(mfrow=c(3,1))
  title=paste("values density for",stnames[i])
  plot(ddensity, col=hexcolors[i], lwd="4", main=paste("Daily",title))
  plot(mdensity, col=hexcolors[i],lwd="4", main=paste("Monthy",title))  
  plot(ydensity, col=hexcolors[i], lwd="4",main=paste("Yearly",title))
  dev.off()
}
# Per type
  # Daily
  name=paste("output/plots/histogramms/daily_density.png", sep="")
  png(filename=name, width=1500, height=1200, units="px")
  par(mfrow=c(3,4))
  for (i in 1:12){
    title=paste("values density for",stnames[i])
    plot(ddensity[[i]], col=hexcolors[i], lwd="4", main=paste("Daily",title))
  }
  dev.off()
  #Monthly
  name=paste("output/plots/histogramms/monthly_density.png", sep="")
  png(filename=name, width=1500, height=1200, units="px")
  par(mfrow=c(3,4))
  for (i in 1:12){
    title=paste("values histogramm for",stnames[i])
    plot(mdensity[[i]], col=hexcolors[i],lwd="4", main=paste("Monthy",title)) 
  }
  dev.off()
  #Yearly
  name=paste("output/plots/histogramms/yearly_density.png", sep="")
  png(filename=name, width=1500, height=1200, units="px")
  par(mfrow=c(3,4))
  for (i in 1:12){
    title=paste("values histogramm for",stnames[i])
    plot(ydensity[[i]], col=hexcolors[i], lwd="4",main=paste("Yearly",title))
  }
  dev.off()
### END DENSITY ###

#### CUMULATIVE SUMS for each station####
dir.create("output/plots/cumulative")
  #create overlapping graphs for all years
  #create common index, here the year 2000 is used (leap year)
  cumlist_s.in=rapply(cumlist, how="list",function(x) zoo(x, order.by=time(cumlist[[1]][[19]])) )
  #plot
  #warnings are usually caused by NA values and then can be ignored
  for (j in 1:length(cumlist_s.in)){
    name=paste("output/plots/cumulative/cumul_overlay_",stnames[j],".png", sep="")
    png(filename=name, width=800, height=600, units="px")
    plot(cumlist_s.in[[j]][[1]], type="l", lwd=2, lty=1, col=hexcolors[[j]], ylim=c(0,5000))
      for (i in 2:31){
        lines(cumlist_s.in[[j]][[i]], type="l", lwd=2, lty=i, col=hexcolors[[j]])
              }
    dev.off()
  }
  rm(cumlist_s.in)
  #long term cumulative sums
  for (i in 1:length(cumsums_ts)) {
    name=paste("output/plots/cumulative/cumul_",stnames[i],".png", sep="")
    png(filename=name, width=800, height=500, units="px")
    plot(cumsums_ts[[i]], type="l",lty=1, lwd=2, col=hexcolors[i], ylab="rainfall in mm", main=paste("Cumulative rainfall amounts for", stnames[i]), xlab="date") 
    dev.off()
  }

### END CUMULATIVE SUMS ###

#### SEASONALITY BOXPLOTS ####
  library(beeswarm)
  for (i in 1:length(m_ts)) { #loop trough station
    name=paste("output/plots/boxplots/m_boxplot_",stnames[i],".png", sep="")
    png(filename=name, width=800, height=600, units="px")
    title=paste("Boxplot of average daily rainfall per month for",stnames[[i]])
    boxplot(bymonth_df_list[[i]], outline=FALSE, ylim=c(0,20), main=title, xlab="month", ylab="mm/day")
    beeswarm(bymonth_ts[[i]], corral="random", pch = 21, col=1, bg=hexcolors[[i]], add=TRUE)
    dev.off()
  }
### END SEASONALITY BOXPLOTS ###

##### 2. TREND ANALYSIS #####
  #### BY MONTH time series with linear trendline ####
  require("zoo")
  dir.create("output/plots/time_series/bymonth") # new directory
  
  #1. Per station: comparison of month within a station
  # Creates a plot matrix with all "by month" time series (Jan 1982, Jan 1983, ..)
  # for each station
  lin_mod=list()
  for (i in 1:length(bymonth_ts)) { #loop trough station
    lin_mod[[i]]=list()
    name=paste("output/plots/time_series/bymonth/ts_",stnames[i],".png", sep="")
    png(filename=name, width=2000, height=1200, units="px")
    par(mfrow=c(4,3))
    for (j in 1:12){ #loop trough month
      lin_mod[[i]][[j]]=lm(bymonth_ts[[i]][[j]]~time(bymonth_ts[[i]][[j]]))
      mname=as.character(format.Date(time(bymonth_ts[[i]][[j]][1]), "%B"))
      title=paste("TS of rainfall sum for",stnames[i],"and month:",mname)
      plot(bymonth_ts[[i]][[j]], type="b", lty=1, lwd=2, col=hexcolors[i], ylab="rainfall in mm", main=title)
      abline(lin_mod[[i]][j]) #trendline
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
  
  #### BY SEASON TIME SERIES ####
  dir.create("output/plots/time_series/byseason") # new directory
  
  # Per station: comparison of Seasons within a station
  # Creates a plot matrix with the season time series (RS 1982, DS 1983, ..)
  # for each station
  for (i in 1:length(rsav_ts)) { #loop trough station
    name=paste("output/plots/time_series/byseason/ts_",stnames[i],".png", sep="")
    png(filename=name, width=1000, height=1200, units="px")
    par(mfrow=c(2,1))
    title=paste("TS of rainfall sum for",stnames[i],"Dry Season")
    plot(dsav_ts[[i]], type="b", lty=1, lwd=2, col=hexcolors[i], ylab="rainfall in mm", main=title)
    abline(lm(dsav_ts[[i]]~time(dsav_ts[[i]]))) #trendline
    title=paste("TS of rainfall sum for",stnames[i],"Rain Season")
    plot(rsav_ts[[i]], type="b", lty=1, lwd=2, col=hexcolors[i], ylab="rainfall in mm", main=title)
    abline(lm(rsav_ts[[i]]~time(rsav_ts[[i]]))) #trendline
    
    dev.off()
  }
  ### END BY SEASON TS ###

### END TREND ANALYSIS ### 


#### COMPARISON WITH ENSO ####

  # Overlapping TS plots (DEPRECIATED)
#   dev.off()
#   plot.new()
#   par(mar=c(5, 4, 4, 6) + 0.1)
# ## Plot SOI plot and put axis scale on right
#   plot(ysoi_ts, xlab="", ylab="", main="PTK11 onthly comparison with equatorial SOI", 
#        ylim=c(-3.5,3.5), xaxt = "n", yaxt = "n", type="l", col="red")
#   polygon(c(min(index(ysoi_ts)), index(ysoi_ts), max(index(ysoi_ts))), 
#           c( 0, soi_ts, 0), density=50, col="red") 
#   abline(0,0, col="red")
#   axis(4, ylim=c(-3,3), col="red",col.axis="red",las=1)
#   mtext("SOI INDEX",side=4,col="red",line=3)
# ## Plot rainfall data and draw its axis
#   par(new=TRUE)
#   ymid=mean(mrs_ts[[2]], na.rm=TRUE)
#   plot(rsav_ts[[2]], ylim=c(ymid-12,ymid+12), 
#        xaxt = "n", yaxt = "n", xlab="", ylab="", 
#        type="l", col="black")
#   abline(ymid,0)
#   axis(2, ylim=c(ymid-12,ymid+12),col="black",las=1)  ## las=1 makes horizontal labels
#   mtext("raifall [mm/day]",side=2,line=2.5)
# ## Draw the time axis
# axis(1,at=time(y_ts[[1]]), labels=format.Date(time(y_ts[[1]]), "%Y"))
# box()

#Scatterplot comparison
dir.create("output/plots/enso")
#plot
for (j in 1:length(m_ts)){
  name=paste("output/plots/enso/soi_monthly_",stnames[j],".png", sep="")
  png(filename=name, width=800, height=600, units="px")
  scatterplot(coredata(m_ts[[j]])~coredata(soi_ts), smoother=FALSE, reg.line=lm,
              main=paste("Comparison between monthly equatorial SOI and monthly rainfall:", stnames[j]), 
              xlab="SOI Index", ylab="rainfall")
    dev.off()
}
for (j in 1:length(rsav_ts)){
  name=paste("output/plots/enso/soi_yearly_",stnames[j],".png", sep="")
  png(filename=name, width=800, height=600, units="px")
  scatterplot(coredata(rsav_ts[[j]])~coredata(ysoi_ts), smoother=FALSE, reg.line=lm,
              main=paste("Comparison between yearly average equatorial SOI and average rainseason rainfall:", stnames[j]), 
              xlab="SOI Index", ylab="rainfall")
  dev.off()
}
### END ENSO ###

#### CLEAN UP ####
rm(name)
graphics.off() #Completely shuts down the printing to file
### END CLEAN UP ###

########## END OF ts_plot.R #############
