###### SPATIO-TEMPORAL RAINFALL PATTERNS IN KAPUAS BASIN ######
	### ANALYSIS AND COMPARISON OF GAUGE DATA ###

## analyse.R
## plots summaries to output files

#### SET UP ####
  source("scripts/setup.R")
  source("scripts/graphic_pars.R")     
###

#### TESTING FOR NORMALITY ####
  # because it is a requirement of some of the homogeneity tests
  source("scripts/convenience_functions.R")
  fpath=("output/quality control")
  dir.create(fpath)
  m.norm.test=norm.test(m_ts)
  write.csv (m.norm.test, file=paste(fpath,"/m.norm.test.csv", sep=""))
  y.norm.test=norm.test(y_ts)
  write.csv (y.norm.test, file=paste(fpath,"/y.norm.test.csv", sep=""))
  y_raindays.norm.test=norm.test(y_raindays)
  write.csv (y_raindays.norm.test, file=paste(fpath,"/y_raindays.norm.test.csv", sep=""))

  rm(fpath, m.norm.test, y.norm.test, y_raindays.norm.test)
###

#### HOMOGENEITY TESTING ####
  source("scripts/convenience_functions.R")
  fpath=("output/quality control")
  dir.create(fpath)
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
  write.csv(hom.tests, file=paste(fpath,"/homogeneity_tests.csv", sep=""))
  rm(rd_hom, ys_hom)
### END HOMOGENEITY TESTING ###

#### SIMPLE TS PLOTS AND SUMMARY STATISTICS ####
  fpath="output/timeseries"
  dir.create(fpath)
  ### PLOT FOR EACH STATION, FOR MEAN VALUE TS 
  tsplot.pst(d_ts, type="daily", fpath=fpath)
  tsplot.pst(w_ts, type="weekly",fpath=fpath)
  tsplot.pst(m_ts, type="monthly",fpath=fpath, ylim=c(0,24))   
  tsplot.pst(y_ts, type="yearly",fpath=fpath)
  tsplot.pst(y_raindays, type="yearly_raindays",fpath=fpath)
rm(fpath)
### TS PLOT of yearly values and raindays ###
  fpath="output/timeseries/double"
  dir.create(fpath)
  double.ts(y_ts, y_raindays, fpath=fpath)
### 
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
# depreceated!!
for (i in 1:length(d_ts))
{
  name=paste(fpath,"/hist_",stnames[i],".png", sep="")
  png(filename=name, pointsize = 11, width=8, height=16, units="cm", res=300)
  par(def.par); par(mfrow=c(3,1), cex.main=0.7)
  title=paste("histogramm and gaussian KDE for",stnames[i])
  ## daily
  truehist(d_ts[[i]][which(d_ts[[i]]>=1)], prob=TRUE, h=5, 
           xlim=c(0,200), ymax=0.05, bty="o", col=colors[i], main=paste("Daily",title))
  rug(jitter(d_ts[[i]][which(d_ts[[i]]>=1)], amount = 0.5))
  lines(ddensity_rd[[i]], lwd=2, col="blue")  
  ## monthly
  truehist(m_ts[[i]], prob=TRUE, h=1.5, 
           xlim=c(0,26), ymax=0.15, bty="o", col=colors[i],  main=paste("Monthly",title))
  rug(m_ts[[i]])
  lines(mdensity[[i]], lwd=2, col="blue")  
  ## yearly
  truehist(y_ts[[i]], prob=TRUE, h=1, 
           xlim=c(2,14),ymax=0.45, lwd=2, col=colors[i], 
           bty="o", main=paste("Yearly",title))
  rug(y_ts[[i]])
  lines(ydensity[[i]], lwd=2, col="blue")
  dev.off()
}
### Per type, gives overview over all stations
  # Daily
  fpath="output/histogramms"
  name=paste(fpath,"/daily_hist.png", sep="")
  png(filename=name, pointsize = 11, width=16, height=10, units="cm", res=300)
  hist.kde.plot(hist.x=d_ts, onlyraindays=TRUE, h=5, xlim=c(-5,190), ymax=0.065, kde.x=ddensity_rd, rug=FALSE)
  dev.off()
  #Monthly
  name=paste(fpath,"/monthly_hist.png", sep="")
  png(filename=name, pointsize = 11, width=16, height=10, units="cm", res=300)
  hist.kde.plot(hist.x=m_ts, h=1.5, xlim=c(-2,27), ymax=0.13, kde.x=mdensity,rug=TRUE)
  dev.off()
  #Yearly
  name=paste(fpath,"/yearly_hist.png", sep="")
  png(filename=name, pointsize = 11, width=16, height=10, units="cm", res=300)
  hist.kde.plot(hist.x=y_ts, h=1, xlim=c(2.5,15.5), ymax=0.45, kde.x=ydensity, rug=TRUE)
  dev.off()
  #Yearly Raindays
  name=paste(fpath,"/yearly_raindays_hist.png", sep="")
  png(filename=name, pointsize = 11, width=16, height=10, units="cm", res=300)
  hist.kde.plot(hist.x=y_raindays, h=15, xlim=c(30,280), ylim=c(0,0.028), kde.x=y_rainday.density, rug=TRUE)
  dev.off()

rm(fpath)
### END HISTOGRAMMS and DENSITY ###

#### COMPARE DENSITIES ####
  fpath="output/histogramms/comparison"
  dir.create(fpath)  
  ## Daily raindays
  name=paste(fpath,"/daily_rd_gauss_kde_densities.png", sep="")
png(filename=name, pointsize = 11, width=16, height=10, units="cm", res=150)
  par(def.par); par(mar=(c(3,3,0.2,0)+0.2))
  plot(ddensity_rd[[1]], xlim=c(0,250), ylim=c(0,0.05), col=colors[1], lwd=2, ylab="frequency", main="", xlab="rainfall (mm/day)")
  for (i in 2:length(ddensity_rd)){ 
    lines(ddensity_rd[[i]], lty=i, col=colors[i], lwd=2)
  }
  legend(x="topright", legend=stnames, lty=(1:length(ddensity_rd)), col=colors, lwd=2, cex=0.7, bty="n")
  dev.off()
## Daily all days
  name=paste(fpath,"/daily_ad_gauss_kde_densities.png", sep="")
  png(filename=name, pointsize = 11, width=16, height=10, units="cm", res=150)
  par(def.par); par(mar=(c(3,3,0.2,0)+0.2))
  plot(ddensity_ad[[1]], xlim=c(0,250), ylim=c(0,0.085), col=colors[1], lwd=2, ylab="frequency", main="", xlab="rainfall (mm/day)")
  for (i in 2:length(ddensity_ad)){ 
    lines(ddensity_ad[[i]], lty=i, col=colors[i], lwd=2)
  }
  legend(x="topright", legend=stnames, lty=(1:length(ddensity_ad)), col=colors, lwd=2, cex=0.7, bty="n")
  dev.off()
  #Monthly
  name=paste(fpath,"/monthly_gauss_kde_densities.png", sep="")
png(filename=name, pointsize = 11, width=16, height=10, units="cm", res=150)
  par(def.par); par(mar=(c(3,3,0.8,0)+0.2))
  plot(mdensity[[1]], xlim=c(0,30), ylim=c(0,0.11), col=colors[1], lwd=2, ylab="frequency", xlab="rainfall (mm/day)", main="")
  for (i in 2:length(ddensity)){ 
    lines(mdensity[[i]], col=colors[i], lwd=2, lty=i)
  }
  legend(x="topright", legend=stnames, lty=(1:length(mdensity)), col=colors, lwd=2, cex=0.7, bty="n")
  dev.off()
  #Yearly
  name=paste(fpath,"/yearly_gauss_kde_densities.png", sep="")
  png(filename=name, pointsize = 11, width=16, height=10, units="cm", res=150)
  par(def.par); par(mar=(c(3,3,0,0)+0.2))
  plot(ydensity[[1]], xlim=c(0,18), ylim=c(0,0.3), col=colors[1], 
       lwd=2, ylab="frequency",xlab="rainfall (mm/day)", main="")          
  for (i in 2:length(ydensity)){ 
    lines(ydensity[[i]], col=colors[i], lwd=2, lty=i)
  }
  legend(x="topright", legend=stnames, lty=(1:length(ydensity)), col=colors, lwd=2, cex=0.7, bty="n")
  dev.off()
  rm(name)
### END COMPARE DENSITIES ###
#### CUMULATIVE DENSITIES####
  fpath="output/histogramms/comparison"
  name=paste(fpath,"/daily_ecdf.png", sep="")
  cuml.plot(d.ecdf, fname=name)
  name=paste(fpath,"/monthly_ecdf.png", sep="")
  cuml.plot(m.ecdf, fname=name)
  name=paste(fpath,"/yearly_ecdf.png", sep="")
  cuml.plot(y.ecdf, fname=name)
### END CUMULATIVE FREQUENCY ###
#### CUMULATIVE SUMS ####
  ### For each station ###
  # -> Virtual Appendix
fpath="output/cumulative"
dir.create(fpath)

#create overlapping graphs for all years
  #create common single index, here the year 2000 is used (leap year)
  cumlist_s.in=rapply(cumlist, how="list", function(x) zoo(x, order.by=time(cumlist[[1]][[19]])) )
  #plot
  #warnings are usually caused by NA values and then can be ignored
  for (j in 1:length(cumlist_s.in)){
    name=paste(fpath,"/cumul_overlay_",stnames[j],".png", sep="")
    png(filename=name, pointsize = 11, width=16, height=9, units="cm", res=300)
    plot(cumlist_s.in[[j]][[1]], type="l", lwd=2, lty=1, col=colors[[j]], ylim=c(0,5000))
      for (i in 2:31){
        lines(cumlist_s.in[[j]][[i]], type="l", lwd=2, lty=i, col=colors[[j]])
              }
    dev.off()
  }
  rm(cumlist_s.in)
#long term cumulative sums
  fpath="output/cumulative"
  for (i in 1:length(cumsums_ts)) {
    name=paste(fpath,"/cumul_",stnames[i],".png", sep="")
    png(filename=name, pointsize = 11, width=16, height=5, units="cm", res=300)
    par(def.par); par(mar=(c(2,2.8,0,0)+0.2));  par(cex.lab=0.7, cex.axis=0.7)
    plot(cumsums_ts[[i]], ylim=c(0,3800), type="l",lty=1, lwd=1, col=colors[i], ylab="rainfall in mm", xlab="", xaxt="n") #main=paste("Cumulative rainfall amounts for", stnames[i]),
    drawTimeAxis(dummy, tick.tstep = "years", lab.tstep = "years", lab.fmt="%Y", cex=0.7) 
    dev.off()
  }

## Comparison of cumsums of average daily values 
  #-> Main Text
  #Cumulative sum of average daily rainfall
  name=paste(fpath,"/av-daily_cumsum.png", sep="")
  png(filename=name, pointsize = 11, width=16, height=9, units="cm", res=300)
  par(def.par);par(mar=(c(2.8,2.8,0.3,0)+0.2)); par(cex.lab=0.7, cex.axis=0.7)
  matplot(d.cumsum_df, xaxt="n", xlim=c(0,366), type = c("l"), lty=c(1:length(d.cumsum_df)), lwd=2, col=colors, xlab="Month", ylab="Cumulative rainfall sum (mm)") 
  axis(1,at=c(0, 31,60,91,121,152,182,213,244,274,305,335,366), labels=c(row.names(davbm_df), ""))
  legend("topleft", legend=colnames(d.cumsum_df), col=colors, lty=c(1:length(d.cumsum_df)), lwd=2,  cex=0.7, bty="n")
  dev.off()
### END CUMULATIVE SUMS ###

#### SEASONALITY PLOTS ####
  fpath="output/seasonality"
  dir.create(fpath)
### MONTHLY AVERAGES ###
  ## LINE PLOT
  # -> main text
    name=paste(fpath, "/lineplot_dav_by_month.png", sep="")
  png(filename=name, pointsize = 11, width=16, height=8, units="cm", res=300)
    par(def.par); par(mar=(c(3,3,0,0)+0.2));  par(cex.lab=0.7, cex.axis=0.7)
    matplot(davbm_df, ylim=c(4,13), type = c("b"), pch=1, lty=(1:length(davbm_df)), lwd=2, cex=0.8, col = colors, xaxt = "n", las=1, ylab="rainfall (mm/day)", xlab="Month")
    axis(1,1:12,labels=row.names(davbm_df))
  legend(x="bottomright", legend=stnames, lty=(1:length(mdensity)), col=colors, lwd=2, cex=0.7, bty="n")
    dev.off()

### MONTHLY VALUES FOR ALL STATIONS, PER MONTH, BOXPLOT VERSION
  ## (variation per month)
  library(beeswarm)
  name=paste(fpath, "/monthly_season_boxplot.png", sep="")
  png(filename=name, pointsize = 11, width=16, height=8, units="cm", res=300)
#     title=paste("Boxplot of average daily rainfall per month for all stations")
    par(def.par); par(mar=(c(2.8,2.8,0,0)+0.2));  par(cex.lab=0.7, cex.axis=0.7)
    boxplot(bymonth_ts_all, ylim=c(0,28), xlim=c(0.8,13.3), cex=0.55, outline=FALSE, xaxt="n", ylim=c(0,28), xlab="Month", ylab="rainfall (mm/day)")
    ## add beeswarm plots
    # first create color vector:
    col.vector=character()
    for (i in 1:12){ #for every month   
        for (j in 1:ncol(bymonth_ts_all[[1]])){  # add as many entries per color (per station) into the vector as there are observations
        col.vector=c(col.vector, rep(colors[j], nrow(bymonth_ts_all[[1]])))} 
    }
    beeswarm(bymonth_ts_all, corral="random", cex=0.5, pwcol=col.vector, add=TRUE)
    axis(1,1:12,labels=row.names(davbm_df))
 legend("topright", legend=stnames, pch=1, col=colors, pt.cex=0.6, cex=0.6, bty="n")
    dev.off()

### SEASONALITY PER STATION
  # -> Virtual Appendix
  fpath="output/seasonality/boxplots"
  dir.create(fpath)
  library(beeswarm)
  for (i in 1:length(m_ts)) { #loop trough station
    name=paste(fpath,"/m_boxplot_",stnames[i],".png", sep="")
    png(filename=name, pointsize = 11, width=16, height=9, units="cm", res=300)
    title=paste("Boxplot of average daily rainfall per month for",stnames[[i]])
    boxplot(bymonth_df_list[[i]], outline=FALSE, ylim=c(0,20), main=title, xlab="month", ylab="mm/day")
    beeswarm(bymonth_ts[[i]], corral="random", pch = 21, col=1, bg=colors[[i]], add=TRUE)
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
  source("scripts/convenience_functions.R")
  # returns only a vector of significance of results
  ## A: seasonal Mann Kendall test. 
    ## runs on monthly values 
    ## SeasonalMannKendall likes only ts objects therefore the as.ts conversion 
    seasonal.mk=mk.trendtest(m_ts, test=SeasonalMannKendall)
    names(seasonal.mk)=paste("seasonal.",names(seasonal.mk), sep="")
  ## B: normal Mann Kendall
    ## on the rainseason and dry season seperately
      # RS
      rs.mk=mk.trendtest(rsav_ts, test=MannKendall)
      names(rs.mk)=paste("rs.",names(rs.mk), sep="")
      # DS
      ds.mk=mk.trendtest(dsav_ts, MannKendall)
      names(ds.mk)=paste("ds.",names(ds.mk), sep="")
  ## Make one table with results
    mk.test=cbind(seasonal.mk, rs.mk, ds.mk)
    write.csv(mk.test, file=paste(fpath,"/mann_kendall_test", sep=""))
  ### END Mann-Kendall trend testing ###
  
  #### BY MONTH time series with linear trendline ####
  fpath="output/timeseries/bymonth/"
  dir.create(fpath) 
  #1. Per station: comparison of month within a station
  # Creates a plot matrix with all "by month" time series (Jan 1982, Jan 1983, ..) for each station
  # title=paste("TS of mean rainfall for",stnames[i],"and month:",mname)
  fpath="output/timeseries/bymonth/stations/"
  dir.create(fpath) 
  lin_mod=list()
  for (i in 1:length(bymonth_ts)) { #loop trough station
    lin_mod[[i]]=list()
    name=paste(fpath,"bymonth_ts_",stnames[i],".png", sep="")
    png(filename=name, pointsize = 11, width=16, height=16, units="cm", res=300)   
    par(def.par); par(mar=c(0,0,1.8,0.4), oma=c(4,4,0,0), las=1); par(cex.main=0.9, adj=0, mgp=c(1.5,0.5,0)); par(mfrow=c(4,3))
    for (j in 1:12){ #loop trough month
      lin_mod[[i]][[j]]=lm(bymonth_ts[[i]][[j]]~time(bymonth_ts[[i]][[j]]))
      mname=as.character(format.Date(time(bymonth_ts[[i]][[j]][1]), "%B"))
      if (j %in% c(1,4,7,10)){ax="s"} else {ax="n"} # axis only on the outside
      plot(bymonth_ts[[i]][[j]], ylim=c(0,25), type="b", cex=0.8, col=colors[i], ylab="", yaxt=ax, xaxt="n", xlab="", main=mname)
      abline(lin_mod[[i]][[j]], lty=1,lwd=1, col=colors[i]) #trendline
      if (j %in% c(10,11,12)){drawTimeAxis(dummy, tick.tstep = "years", lab.tstep = "years", lab.fmt="%Y", cex=0.8)}
    }
    mtext("rainfall (mm/day)", side = 2, line = 2.5, cex=0.8, las=0, outer = TRUE, at = NA,  adj = 0.5, padj = 0.5)
    mtext("Time", side = 1, line =2, cex=0.8, outer = TRUE, adj = 0.5, padj = 0.5)
    dev.off()
  }

  # 2. Per month: comparison of stations for every month
  # Creates a plot matrix for 8 stations of a particular month  of the "by month" time series (Jan 1982, Jan 1983, ..)
  #title=paste("TS of mean rainfall for",stnames[i],"and month:",mname)
  fpath="output/timeseries/bymonth/per_month/"
  dir.create(fpath) 
  for (j in 1:12){  # loop through month
    mname=as.character(format.Date(time(bymonth_ts[[1]][[j]][1]), "%B"))
    name=paste(fpath,"bymonth_ts_",mname,".png", sep="")
    png(filename=name, pointsize = 11, width=16, height=16, units="cm", res=300)  
    par(def.par); par(mar=c(0,0,1.8,0.4), oma=c(4,4,0,0), las=1); par(cex.main=0.9, adj=0, mgp=c(1.5,0.5,0))
    par(mfrow=c(round(length(bymonth_ts)/3),3)) #a*b=length(bymonth_ts)
    for (i in 1:length(bymonth_ts)) {    #loop trough stations
      if (i %in% seq(1, length(bymonth_ts),3)){ax="s"} else {ax="n"} # axis only on the outside
      plot(bymonth_ts[[i]][[j]], ylim=c(0,25), type="b", cex=0.8, col=colors[i], yaxt=ax,ylab="", xaxt="n", xlab="", main=stnames[i]) #ylab="rainfall (mm/day)", xlab="Time")
      abline(lm(bymonth_ts[[i]][[j]]~time(bymonth_ts[[i]][[j]])),lty=1,lwd=1, col=colors[i]) #trendline
      if (i %in% c((round(length(bymonth_ts)/3)*3-3):length(bymonth_ts))){drawTimeAxis(dummy, tick.tstep = "years", lab.tstep = "years", lab.fmt="%Y", cex=0.7)} # x axis only on the bottom
    }
    mtext("rainfall (mm/day)", side = 2, line = 2.5, cex=0.8, las=0, outer = TRUE, at = NA,  adj = 0.5, padj = 0.5)
    mtext("Time", side = 1, line =2, cex=0.8, outer = TRUE, adj = 0.5, padj = 0.5)
    dev.off()
  }
  ### END BY MONTH TS ###
### END TREND ANALYSIS ### 

#### BY SEASON TIME SERIES ####
  # -> virtual appendix
  # Per station: comparison of Seasons within a station
  # Creates a plot with the season time series (RS 1982, DS 1983, ..)
  # for each station  
  fpath="output/seasonality"
  dir.create(fpath) # new directory

  for (i in 1:length(rsav_ts)) { #loop trough station
    name=paste(fpath,"/seasonal_ts_",stnames[i],".png", sep="")
    png(filename=name, pointsize = 11, width=16, height=9, units="cm", res=300)
    par(def.par); par(mar=(c(1.8,2.8,0,0)+0.2))
#     title=paste("TS of rainfall in wet and dry season for",stnames[i])
    plot(rsav_ts[[i]], ylim=c(0,17), xaxt="n", las=1, type="b", pch=19, cex=0.8, lty=1, lwd=2, col=colors[i], ylab="rainfall (mm/day)", xlab="")
    abline(mean(rsav_ts[[i]], na.rm=TRUE),0,lty=1, lwd=1, 
           col=colors[i]) #mean line
    #abline(lm(rsav_ts[[i]]~time(rsav_ts[[i]])),lty=1, lwd=2) #trendline
    lines(dsav_ts[[i]], type="b", cex=0.8, lty=3, lwd=2, col=colors[i])
    abline(mean(dsav_ts[[i]], na.rm=TRUE),0,lty=3, lwd=2, 
           col=colors[i]) #mean line
    #abline(lm(dsav_ts[[i]]~time(dsav_ts[[i]])), lty=3, lwd=2) #trendline 
    drawTimeAxis(dummy, tick.tstep = "years", lab.tstep = "years", lab.fmt="%Y", cex=0.7) 
    legend("bottomright", legend=c("rainy season (Nov-Jan)", "dry season (Jun-Aug)"), lty=c(1,3), col=colors[i], lwd=2, cex=0.7, bty="n")
    
    dev.off()
  }
  ### END BY SEASON TS ###

#### CLEAN UP ####
  rm(name, fpath)
  graphics.off() #Completely shuts down the printing to file
###

###### END analyse.R ######
