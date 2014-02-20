###### PRECIPITATION ANALYSIS: COMPARISON OF GROUND DATA ######

## compare.R plots spatial and comparative summaries to output files ##

#### SET UP ####
  source("scripts/setup.R")
  source("scripts/graphic_pars.R")
### END SET UP ###

#### Box plot for station comparison ####
  library(beeswarm)
  fpath="output/boxplots"
  dir.create(fpath)
# year
    name=paste(fpath,"/yearly_boxplot.svg", sep="")
    svg(filename=name, width=(16/2.54)*1.5, height=(8/2.54)*1.5, pointsize = 10)
    par(def.par)
    par(cex.axis=0.95, mar=(c(4,4,0.3,0)+0.2))
    boxplot(y_df, outline=FALSE, ylab="rainfall in mm/day", xaxt="n")
    beeswarm(y_ts, pch=21, bg=colors, cex=1.3, add=TRUE)
    abline(mean(y_df,  na.rm=TRUE),0, lwd=2.8, lty=2, col="#dd4444")
    axis(1, at=(1:14), labels = FALSE)
    text(1:14, par("usr")[3] - 0.25, srt = 35, adj = 1.05,
    labels = colnames(y_df), xpd = TRUE, cex=0.95)
    dev.off()

#monthly per month i.e. Jan
    name=paste(fpath,"/monthly_boxplot.png", sep="")
    png(filename=name, width=2000, height=1200, units="px")
    par(mfrow=c(4,3))
    for (j in 1:12){ #loop trough month
    month=if (j>=10){j} else {paste("0",j, sep="")}
    selector <- format.Date(as.Date(row.names(m_df)),format="%m")
    mname=as.character(format.Date(time(m_ts[[1]][j]), "%B"))
    title=paste("Average daily rainfall in",mname)
    boxplot(m_df[selector==month,], main=title, xlab="Station", ylab="mm/day")
    abline(mean(m_df[selector==month,], na.rm=TRUE),0, lwd="2", col="blue")
    }
  dev.off()
  rm(fpath)
### END BOX PLOTS ###

#### ALL STATION TIME SERIES IN ONE PLOT ####
  ### Monthly TS ###
  name="output/timeseries/monthly_ts.png"
  png(filename=name, width=1000, height=700, units="px")
  matplot(m_df, type = c("l"),pch=1, lwd=2, lty=c(1), col = colors, xaxt = "n", ylab="rainfall in mm/year", main="Yearly Time Series", xlab="Year")
  axis(1,1:372,labels=substr(row.names(m_df),1,7))  
  legend(x="bottomleft", legend=stnames, col=colors, lwd=3, cex=0.8)
  dev.off()
  
  ### YEARLY TS ###
  name="output/timeseries/yearly_ts.png"
  png(filename=name, width=1000, height=700, units="px")
  matplot(y_df, type = c("b"),pch=1, lwd=2, lty=c(1), col = colors, xaxt = "n", ylab="rainfall in mm/year", main="Yearly Time Series", xlab="Year")
  axis(1,1:31,labels=substr(row.names(y_df),1,4))  
  legend(x="bottomright", legend=stnames, col=colors, lwd=3, cex=0.8)
  dev.off()
  
### END MONTHLY AVERAGES ###



#### CORRELATION ####
  fpath="output/correlation"
  dir.create(fpath)
  #### CORRELOGRAMS ####
    fpath="output/correlation/correlograms"
    dir.create(fpath)
    
    corgr(d_df, type="daily", fpath=fpath)
    corgr(w_df, type="weekly", fpath=fpath)
    corgr(m_df, type="monthly", fpath=fpath)
    corgr(y_df, type="yearly", fpath=fpath) #currently not enough values
    
    corgr(rs_df, type="rainseason daily", fpath=fpath)
    corgr(ds_df, type="dryseason daily", fpath=fpath)
    corgr(mrs_df, type="rainseason monthly", fpath=fpath)
    corgr(mds_df, type="dryseason monthly", fpath=fpath)
    
    rm(fpath)
  ### END CORRGRAMS ###
  #### SCATTERPLOT MATRIX ####
    fpath="output/correlation/scatterplotmatrix"
    dir.create(fpath)
    
    scatterMatrix(d_df, xylim=c(0,150), type="daily", fpath=fpath)
    scatterMatrix(w_df, xylim=c(0,45), type="weekly", fpath=fpath)
    scatterMatrix(m_df, xylim=c(0,25),type="monthly", fpath=fpath)
    scatterMatrix(y_df, xylim=c(0,20), type="yearly", fpath=fpath) #currently not enough values
    
    scatterMatrix(rs_df, xylim=c(0,150),type="rainseason daily", fpath=fpath)
    scatterMatrix(ds_df, xylim=c(0,150),type="dryseason daily", fpath=fpath)
    scatterMatrix(mrs_df, xylim=c(0,25),type="rainseason monthly", fpath=fpath)
    scatterMatrix(mds_df, xylim=c(0,25),type="dryseason monthly", fpath=fpath)
    
    rm(fpath)
  ### END SCATTERPLOT MATRIX ###
### END CORRELATION###
# #### Cumulative Sums COMPARISON####
#   dir.create("output/plots/cumulative")
# 
# ### Comparison of cumulative sums ###
# ## 1. Comparison of SGU 1, 19, 17 since they are spatially close
# #10year 
#   name=paste("output/plots/cumulative/10ycumsum_comparison.png", sep="")
#   png(filename=name, width=800, height=500, units="px")
#   matplot(cumfun_df[6575:10592,c(3,7,8)], type = c("l"), xaxt="n",  pch=1, lwd=2, lty=c(1), col =colors[c(3,7,8)], ylab="rainfall in mm/year", main=paste("Cumulative rainfall amounts for", stnames[c(3,7,8)]), xlab="Year")
#   axis(1,at=seq(0, 4015, by=365), labels=c(2000:2011)  )
#   legend(x="topright", legend=stnames[c(3,7,8)], col=colors[c(3,7,8)], lwd=3, cex=0.8)
#   dev.off()
# #2 year
#   name=paste("output/plots/cumulative/2ycumsum_comparison.png", sep="")
#   png(filename=name, width=800, height=500, units="px")
#   matplot(cumfun_df[7671:8402,c(3,7,8)], type = c("l"), xaxt="n",  pch=1, lwd=2, lty=c(1), col =colors[c(3,7,8)], ylab="rainfall in mm/year", main=paste("Cumulative rainfall amounts for", stnames[c(3,7,8)]), xlab="Year")
#   axis(1,at=seq(0, 740, by=365), labels=c(2003:2004)  )
#   legend(x="topright", legend=stnames[c(3,7,8)], col=colors[c(3,7,8)], lwd=3, cex=0.8)
#   dev.off()
# ## 2. Comparison of PTK11, SGU01, STG01 KPH01, in order to see west east gradient
# #3 year
#   name=paste("output/plots/cumulative/3ycumsum_ew_comparison.png", sep="")
#   png(filename=name, width=1000, height=500, units="px")
#   stnamestring=paste(stnames[c(1,3,9,11)], collapse=" ")
#   matplot(cumfun_df[7671:8767,c(1,3,9,11)], type = c("l"), xaxt="n",  pch=1, lwd=2, lty=c(1), col =colors[c(1,3,9,11)], ylab="rainfall in mm/year", main=paste("Cumulative rainfall amounts for",stnames[c(1,3,9,11)], collapse=" "), xlab="Year")
#   axis(1,at=seq(0, 1105, by=365), labels=c(2003:2006)  )
#   legend(x="topleft", legend=stnames[c(1,3,9,11)], col=colors[c(1,3,9,11)], lwd=3, cex=0.8)
#   dev.off()
# ### END Cumulative Sums ###
# 
# ### Comparison PTK11- KPH01 ###
#   plot(window(m_ts[[1]], start=as.Date('2002-01-01'), end=as.Date('2010-12-31')),  type="l", lty=1, lwd=2, col=colors[1], ylab="rainfall in mm/day", main=paste("Time series of daily rainfall amounts for",stnames[c(1,11)], collapse=" "), xlab="Time") 
#   lines(window(m_ts[[11]], start=as.Date('2002-01-01'), end=as.Date('2010-12-31')), col=colors[11])
# 
# ###  ###

#### COMPARISON OF SPATIAL CORRELATION AND DISTANCE ####
library("sp")
library("maptools")
library("raster")
library("reshape")
source("scripts/convenience_functions.R")
# Load station data (not in long lat to get distances in meters)
  stations<-readShapePoints("input/stationmap_utm49N/stationmap_utm49N.shp")
# Subset stations 
  stations <- stations[stations$ID %in% stnames,]
# Compute distances
  sp.dist.matrix=pointDistance(stations, longlat=FALSE)
  sp.dist.matrix=sp.dist.matrix/1000 #to get km instead of m
  rownames(sp.dist.matrix)=stations$ID
  colnames(sp.dist.matrix)=stations$ID
# reorder
  sp.dist=reshape.matrix(sp.dist.matrix)

# correlation matrixes
  cor.matrix_d=cor(d_df, use="pairwise.complete.obs", method ="pearson")
  cor_d=reshape.matrix(cor.matrix_d)

  cor.matrix_m=cor(m_df, use="pairwise.complete.obs", method ="pearson")
  cor_m=reshape.matrix(cor.matrix_m)

  cor.matrix_y=cor(y_df, use="pairwise.complete.obs", method ="pearson")
  cor_y=reshape.matrix(cor.matrix_y)

# Plot
plot(cor_d$value~sp.dist$value, ylim=c(0,1), lty=2, xlim=c(0,300),   ylab="Pearson correlation coefficient", xlab="distance in km")
plot(cor_m$value~sp.dist$value, ylim=c(0,1), lty=2, xlim=c(0,300))
plot(cor_y$value~sp.dist$value, ylim=c(0,1), lty=2, xlim=c(0,300))

  rm(sp.dist.matrix, cor.matrix_d, cor.matrix_m, cor.matrix_y)
### END COMPARISON OF SPATIAL CORRELATION AND DISTANCE ###

#### shut down ####
rm(name, fpath)
graphics.off() #Completely shuts down the printing to file
### END SHUT DOWN ###

########## END OF sp_plot.R #############
