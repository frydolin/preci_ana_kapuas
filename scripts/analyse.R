###### PRECIPITATION ANALYSIS: COMPARISON OF STATION DATA ######

## analyse.R plots summaries to output files ##

#### SET UP ####
source("scripts/setup.R")
### END SET UP ###

#### HOMOGENEITY TESTING ####
  source("scripts/convenience_functions.R")
  rd_hom=homogeneity.tests(y_raindays)
  ys_hom=homogeneity.tests(y_ts)
  # make a data frame
  rd_hom=as.data.frame(rd_hom)
  colnames(rd_hom)=paste("rd.",colnames(rd_hom), sep="")
  ys_hom=as.data.frame(ys_hom)
  colnames(ys_hom)=paste("ys.",colnames(ys_hom), sep="")
  hom.tests=cbind(rd_hom[1], ys_hom[1], rd_hom[2], ys_hom[2], rd_hom[3], ys_hom[3],
                  rd_hom[4], ys_hom[4])
  row.names(hom.tests)=stnames
  write.csv(hom.tests, file="output/homogeneity_tests")
  rm(rd_hom, ys_hom)
### END HOMOGENEITY TESTING ###

#### SIMPLE TS PLOTS AND SUMMARY STATISTICS ####
  fpath="output/timeseries"
  dir.create(fpath)
  ### PLOT FOR EACH STATION, FOR MEAN VALUE TS 
  tsplot.pst(d_ts, type="daily", fpath=fpath)
  tsplot.pst(w_ts, type="weekly",fpath=fpath)
  tsplot.pst(m_ts, type="monthly",fpath=fpath)   
  tsplot.pst(y_ts, type="yearly",fpath=fpath)
rm(fpath)
  ### SUMMARY STATISTICS FOR SUM VALUES
fpath="output/summaries"
dir.create(fpath)
  daily_summary=(make.smry(d_ts))
  write.csv(daily_summary, file=paste(fpath,"/daily_summary.csv", sep=""), na = "NA")
  weekly_summary=make.smry(ws_ts)
  write.csv(weekly_summary, file=paste(fpath,"/weekly_summary.csv", sep=""), na = "NA")
  monthly_summary=make.smry(ms_ts)
  write.csv(monthly_summary, file=paste(fpath,"/monthly_summary.csv", sep=""), na = "NA")
  yearly_summary=make.smry(ys_ts)
  write.csv(yearly_summary, file=paste(fpath,"/yearly_summary.csv", sep=""), na = "NA")
rm(fpath)
### END TS per Station ###

#### HISTOGRAMS and DENSITY####
  fpath="output/histogramms"
  dir.create(fpath) # new directory
library("MASS")
### Per station
for (i in 1:length(d_ts))
{
  name=paste(fpath,"/hist_",stnames[i],".png", sep="")
  png(filename=name, width=700, height=1000, units="px")
  par(mfrow=c(3,1))
  title=paste("histogramm and gaussian KDE for",stnames[i])
  ## daily
  truehist(d_ts[[i]][which(d_ts[[i]]>=1)], prob=TRUE, h=5, 
           xlim=c(0,200), ymax=0.05, bty="o", col=hexcolors[i], main=paste("Daily",title))
  rug(jitter(d_ts[[i]][which(d_ts[[i]]>=1)], amount = 0.5))
  lines(ddensity[[i]], lwd=3, col="blue")  
  ## monthly
  truehist(m_ts[[i]], prob=TRUE, h=1.5, 
           xlim=c(0,26), ymax=0.15, bty="o", col=hexcolors[i],  main=paste("Monthly",title))
  rug(m_ts[[i]])
  lines(mdensity[[i]], lwd=3, col="blue")  
  ## yearly
  truehist(y_ts[[i]], prob=TRUE, h=1, 
           xlim=c(2,14),ymax=0.45, lwd=2, col=hexcolors[i], 
           bty="o", main=paste("Yearly",title))
  rug(y_ts[[i]])
  lines(ydensity[[i]], lwd=3, col="blue")
  dev.off()
}
### Per type
  # Daily
  name=paste(fpath,"/daily_hist.png", sep="")
  png(filename=name, width=1500, height=1200, units="px")
  par(mfrow=c(3,4))
  for (i in 1:12){
    title=paste("histogramm and gaussian KDE for",stnames[i])
    truehist(d_ts[[i]][which(d_ts[[i]]>=1)], prob=TRUE, h=5, 
             xlim=c(0,200), ymax=0.05, bty="o", col=hexcolors[i], main=paste("Daily",title))
    rug(jitter(d_ts[[i]][which(d_ts[[i]]>=1)], amount = 0.5))
    lines(ddensity[[i]], lwd=3, col="blue")  
  }
  dev.off()
  #Monthly
  name=paste(fpath,"/monthly_hist.png", sep="")
  png(filename=name, width=1500, height=1200, units="px")
  par(mfrow=c(3,4))
  for (i in 1:length(d_ts)){
    title=paste("histogramm and gaussian KDE for",stnames[i])
    truehist(m_ts[[i]], prob=TRUE, h=1.5, 
             xlim=c(0,26), ymax=0.15, bty="o", col=hexcolors[i],  main=paste("Monthly",title))
    rug(m_ts[[i]])
    lines(mdensity[[i]], lwd=3, col="blue") 
  }
  dev.off()
  #Yearly
  name=paste(fpath,"/yearly_hist.png", sep="")
  png(filename=name, width=1500, height=1200, units="px")
  par(mfrow=c(3,4))
  for (i in 1:length(d_ts)){
    title=paste("histogramm and gaussian KDE for",stnames[i])
    truehist(y_ts[[i]], prob=TRUE, h=1, 
             xlim=c(2,14),ymax=0.45, lwd=2, col=hexcolors[i], 
             bty="o", main=paste("Yearly",title))
    rug(y_ts[[i]])
    lines(ydensity[[i]], lwd=3, col="blue")
  }
  dev.off()
#   #Yearly Raindays
#   name=paste(fpath,"/yearly_raindays_hist.png", sep="")
#   png(filename=name, width=1500, height=1200, units="px")
#   par(mfrow=c(3,4))
#   for (i in 1:12){
#     title=paste("histogramm and gaussian KDE for",stnames[i])
#     truehist(y_raindays[[i]], prob=TRUE, h=10, 
#              xlim=c(0,300),ymax=0.04, lwd=2, col=hexcolors[i], 
#              bty="o", main=paste("Yearly",title))
#     rug(y_raindays[[i]])
#     lines(y_rainday.density[[i]], lwd=3, col="blue")
#   }
#   dev.off()

rm(fpath)
### COMPARE DENSITIES ###
  fpath="output/histogramms/comparison"
  dir.create(fpath)  
  ## Daily
  name=paste(fpath,"/daily_densities.png", sep="")
  png(filename=name, width=1000, height=700, units="px")
  plot(ddensity[[1]], xlim=c(0,250), ylim=c(0,0.05), col=hexcolors[1], 
       lwd="3", xlab="rainfall in mm/day", main="Gaussian KDE  of daily rainfall")
  for (i in 2:length(ddensity)){ 
    lines(ddensity[[i]], col=hexcolors[i], lwd="3")
  }
  legend(x="topright", legend=stnames, col=hexcolors, lwd=3, cex=0.8)
  dev.off()
  #Monthly
  name=paste(fpath,"/monthly_densities.png", sep="")
  png(filename=name, width=1000, height=700, units="px")
  plot(mdensity[[1]], xlim=c(0,30), ylim=c(0,0.11), col=hexcolors[1], 
       lwd="3", xlab="rainfall in mm/day", main="Gaussian KDE  of monthly rainfall")          
  for (i in 2:length(ddensity)){ 
    lines(mdensity[[i]], col=hexcolors[i], lwd="3")
  }
  legend(x="topright", legend=stnames, col=hexcolors, lwd=3, cex=0.8)
  dev.off()
  #Yearly
  name=paste(fpath,"/yearly_densities.png", sep="")
  png(filename=name, width=1000, height=700, units="px")
  plot(ydensity[[1]], xlim=c(0,20), ylim=c(0,0.3), col=hexcolors[1], 
       lwd="3", xlab="rainfall in mm/day", main="Gaussian KDE of yearly rainfall")          
  for (i in 2:length(ddensity)){ 
    lines(ydensity[[i]], col=hexcolors[i], lwd="3")
  }
  legend(x="topright", legend=stnames, col=hexcolors, lwd=3, cex=0.8)
  dev.off()
  ### END COMPARE DENSITIES ###
### END HISTOGRAMMS and DENSITY ###

#### CUMULATIVE SUMS for each station####
fpath="output/cumulative"
dir.create(fpath)
  #create overlapping graphs for all years
  #create common index, here the year 2000 is used (leap year)
  cumlist_s.in=rapply(cumlist, how="list",function(x) zoo(x, order.by=time(cumlist[[1]][[19]])) )
  #plot
  #warnings are usually caused by NA values and then can be ignored
  for (j in 1:length(cumlist_s.in)){
    name=paste(fpath,"/cumul_overlay_",stnames[j],".png", sep="")
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
    name=paste(fpath,"/cumul_",stnames[i],".png", sep="")
    png(filename=name, width=800, height=500, units="px")
    plot(cumsums_ts[[i]], type="l",lty=1, lwd=2, col=hexcolors[i], ylab="rainfall in mm", main=paste("Cumulative rainfall amounts for", stnames[i]), xlab="date") 
    dev.off()
  }
  rm(fpath)
### END CUMULATIVE SUMS ###

#### SEASONALITY PLOTS ####
  fpath="output/seasonality"
  dir.create(fpath)
  
### MONTHLY AVERAGES ###
  ## LINE PLOT
    name=paste(fpath, "/dav_by_month.png", sep="")
    png(filename=name, width=1000, height=700, units="px")  
    matplot(davbm_df, type = c("b"),pch=1, lty=c(1), lwd=2, col = hexcolors, xaxt = "n", ylab="rainfall in mm/day", main="Daily Average Rain per Month", xlab="Month")
    axis(1,1:12,labels=row.names(davbm_df))
    legend(x="bottomright", legend=stnames, col=hexcolors, lwd=3, cex=0.8)
    dev.off()
#   ## AVERAGES BOX PLOT VERSION
#    #the plot below is somehow an improved version instead of comparing averages
#     name="output/seasonality/dav_by_month_boxplot.png"
#     png(filename=name, width=1000, height=700, units="px")
#     boxplot(t(davbm_df), outline=TRUE, main="Station average of daily average Rain per Month", xlab="Month", ylab="mm/day")
#     abline(mean(t(davbm_df), na.rm=TRUE),0, lwd="2", col="blue")
#     dev.off()

### MONTHLY VALUES FOR ALL STATIONS, PER MONTH, BOXPLOT VERSION
  ## (variation per month)
  library(beeswarm)
    name=paste(fpath, "/monthly_boxplot.png", sep="")
    png(filename=name, width=1000, height=700, units="px")  
    title=paste("Boxplot of average daily rainfall per month for all stations")
    boxplot(bymonth_ts_all, outline=FALSE, xaxt="n", ylim=c(0,28), main=title, xlab="month", ylab="mm/day")
    #add beeswarm plots
    # first create color vector:
    #for every month
    # add as many entries per color into the vector as there are observations
    col.vector=character()
    for (i in 1:12){       
        for (j in 1:ncol(bymonth_ts_all[[1]])){ 
        col.vector=c(col.vector, rep(hexcolors[j], nrow(bymonth_ts_all[[1]])))
      } }
    beeswarm(bymonth_ts_all, corral="random", pch = 21, pwcol=col.vector, add=TRUE)
    axis(1,1:12,labels=row.names(davbm_df))
    legend(x="bottomright", legend=stnames, col=hexcolors, lty=1, lwd=3, cex=0.8)
    dev.off()
    
### SEASONALITY PER STATION
  fpath="output/seasonality/boxplots"
  dir.create(fpath)
  library(beeswarm)
  for (i in 1:length(m_ts)) { #loop trough station
    name=paste(fpath,"/m_boxplot_",stnames[i],".png", sep="")
    png(filename=name, width=800, height=600, units="px")
    title=paste("Boxplot of average daily rainfall per month for",stnames[[i]])
    boxplot(bymonth_df_list[[i]], outline=FALSE, ylim=c(0,20), main=title, xlab="month", ylab="mm/day")
    beeswarm(bymonth_ts[[i]], corral="random", pch = 21, col=1, bg=hexcolors[[i]], add=TRUE)
    dev.off()
  }
  rm(fpath)

### END SEASONALITY PLOTS ###

##### TREND ANALYSIS #####
fpath="output/trendanalysis"
dir.create(fpath) # new directory

  #### Mann-Kendall trend testing ####
  library("Kendall")
  # testing is outsourced to convenience function mk.trendtest
  # returns only a vector of significance of results
  ## A: seasonal Mann Kendall test. 
    ## runs on monthly values 
    ## SeasonalMannKendall likes only ts objects therefore the as.ts conversion 
    ## tau=Score/denominator, denominator=max possible value for score
    seasonal.mk=mk.trendtest(m_ts, SeasonalMannKendall)
    names(seasonal.mk)=paste("seasonal.",names(seasonal.mk), sep="")
  ## B: normal Mann Kendall
    ## on the rainseason and dry season seperately
      # RS
      rs.mk=mk.trendtest(rsav_ts, MannKendall)
      names(rs.mk)=paste("rs.",names(rs.mk), sep="")
      # DS
      ds.mk=mk.trendtest(dsav_ts, MannKendall)
      names(ds.mk)=paste("ds.",names(ds.mk), sep="")
  ## Make one table with results
    mk.test=cbind(seasonal.mk, rs.mk, ds.mk)
    write.csv(mk.test, file=paste(fpath,"/mann_kendall_test", sep=""))
  ### END Mann-Kendall trend testing ###
  
  #### BY MONTH time series with linear trendline ####
  dir.create(paste(fpath,"/bymonth", sep="")) # new directory
  
  #1. Per station: comparison of month within a station
  # Creates a plot matrix with all "by month" time series (Jan 1982, Jan 1983, ..)
  # for each station
  lin_mod=list()
  for (i in 1:length(bymonth_ts)) { #loop trough station
    lin_mod[[i]]=list()
    name=paste(fpath,"/bymonth/ts_",stnames[i],".png", sep="")
    png(filename=name, width=2000, height=1200, units="px")
    par(mfrow=c(4,3))
    for (j in 1:12){ #loop trough month
      lin_mod[[i]][[j]]=lm(bymonth_ts[[i]][[j]]~time(bymonth_ts[[i]][[j]]))
      mname=as.character(format.Date(time(bymonth_ts[[i]][[j]][1]), "%B"))
      title=paste("TS of mean rainfall for",stnames[i],"and month:",mname)
      plot(bymonth_ts[[i]][[j]], type="b", lty=1, lwd=2, col=hexcolors[i], ylab="rainfall in mm/day", main=title)
      abline(lin_mod[[i]][j]) #trendline
    }
    dev.off()
  }
  
  # 2. Per month: comparison of stations for every month
  # Creates a plot matrix for all stations of a particular month 
  # of the "by month" time series (Jan 1982, Jan 1983, ..)
  for (j in 1:12){  # loop through month
    mname=as.character(format.Date(time(bymonth_ts[[1]][[j]][1]), "%B"))
    name=paste(fpath,"/bymonth/ts_",mname,".png", sep="")
    png(filename=name, width=2000, height=1200, units="px")
    par(mfrow=c(4,3))
    for (i in 1:12) {    #loop trough first twelve stations
      title=paste("TS of mean rainfall for",stnames[i],"and month:",mname)
      plot(bymonth_ts[[i]][[j]], type="b", lty=1, lwd=2, col=hexcolors[i], ylab="rainfall in mm/day", main=title)
      abline(lm(bymonth_ts[[i]][[j]]~time(bymonth_ts[[i]][[j]]))) #trendline
    }
    dev.off()
  }
  ### END BY MONTH TS ###

### END TREND ANALYSIS ### 

#### BY SEASON TIME SERIES ####
  dir.create(paste(fpath,"/byseason", sep="")) # new directory
  
  # Per station: comparison of Seasons within a station
  # Creates a plot matrix with the season time series (RS 1982, DS 1983, ..)
  # for each station
  
  for (i in 1:length(rsav_ts)) { #loop trough station
    name=paste(fpath,"/byseason/ts_",stnames[i],".png", sep="")
    png(filename=name, width=800, height=600, units="px")
    title=paste("TS of rainfall in wet and dry season for",stnames[i])
    plot(rsav_ts[[i]], ylim=c(0,18), xaxt="n", type="b", lty=1, lwd=2, 
         col=hexcolors[i], ylab="rainfall in mm", main=title)
    abline(mean(rsav_ts[[i]], na.rm=TRUE),0,lty=1, lwd=2, 
           col=hexcolors[i]) #mean line
    #abline(lm(rsav_ts[[i]]~time(rsav_ts[[i]])),lty=1, lwd=2) #trendline
    
    lines(dsav_ts[[i]], type="b", lty=3, lwd=2, col=hexcolors[i])
    abline(mean(dsav_ts[[i]], na.rm=TRUE),0,lty=3, lwd=2, 
           col=hexcolors[i]) #mean line
    #abline(lm(dsav_ts[[i]]~time(dsav_ts[[i]])), lty=3, lwd=2) #trendline
    
    drawTimeAxis(dummy, tick.tstep = "years", lab.tstep = "years", lab.fmt="%Y") 
    legend("bottomleft", legend=c("RAINY", "DRY"), lty=c(1,3), col=hexcolors[i], lwd=1)
    
    dev.off()
  }
  ### END BY SEASON TS ###
rm(fpath)


#### CLEAN UP ####
rm(name, fpath)
graphics.off() #Completely shuts down the printing to file
### END CLEAN UP ###

########## END OF ts_plot.R #############
