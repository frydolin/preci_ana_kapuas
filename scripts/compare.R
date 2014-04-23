###### SPATIO-TEMPORAL RAINFALL PATTERNS IN KAPUAS BASIN ######
	### COMPARISON OF GROUND DATA ###

## compare.R plots spatial and comparative summaries to output files ##

#### SET UP ####
  source("scripts/setup.R")
  source("scripts/graphic_pars.R")
  source("scripts//convenience_functions.R")
### END SET UP ###

#### BOX PLOT STATION COMPARISON ####
  fpath="output/boxplot_comparison"
  dir.create(fpath)
## yearly values and yearly raindays combined
    name=paste(fpath,"/yearly_mm_rd_boxplot.png", sep="")
    png(filename=name, pointsize = 11, width=16, height=13, units="cm", res=300)
  par(def.par); par(mfrow=c(2,1)) 
    bplot.bswarm(ts=y_ts, df=y_df, ylab="annual mean rainfall (mm/day)", xlabel=FALSE)
  mtext("A", side = 3, line = 0, cex=1, las=1, adj=0, padj=0)
    bplot.bswarm(ts=y_raindays, df=y_raindays_df, ylab="annual number of raindays", xlabel=TRUE)
  mtext("B", side = 3, line = 0, cex=1, las=1, adj=0, padj=0)
  dev.off()
##
## yearly values seperately
  name=paste(fpath,"/yearly_boxplot.png", sep="")
  png(filename=name, pointsize = 11, width=16, height=8, units="cm", res=300);  par(def.par)
  bplot.bswarm(ts=y_ts, df=y_df, ylab="annual rainfall (mm/day)", xlabel=TRUE)
  dev.off()
## yearly raindays seperately
  name=paste(fpath,"/yearly_raindays_boxplot.png", sep="")
  png(filename=name, pointsize = 11, width=16, height=8, units="cm", res=300); par(def.par)
  bplot.bswarm(ts=y_raindays, df=y_raindays_df, ylab="annual no. of raindays", xlabel=TRUE)
  dev.off()
##
## Monthly per month i.e. Jan, Feb
    name=paste(fpath,"/monthly_boxplot.png", sep="")
    png(filename=name, pointsize = 11, width=20, height=20, units="cm", res=300)
    par(def.par); par(mar=(c(2.8,2.8,1,0)+0.2));par(mfrow=c(4,3))
    for (j in 1:12){ #loop trough month
    month=if (j>=10){j} else {paste("0",j, sep="")}
    selector <- format.Date(as.Date(row.names(m_df)),format="%m")
    mname=as.character(format.Date(time(m_ts[[1]][j]), "%B"))
    boxplot(m_df[selector==month,], ylim=c(0,28), main=mname, cex.main=0.8, adj=0, xaxt="n", xlab="", ylab="rainfall (mm/day)")
    abline(mean(m_df[selector==month,], na.rm=TRUE),0, lwd=2, lty=3, col="darkred")
    axis(1, at=(1:ncol(m_df)), labels = FALSE)
    text(1:ncol(m_df), par("usr")[3] - 0.6, srt = 35, adj = 1.05, labels = colnames(m_df), xpd = TRUE, cex=0.7)
    }
  dev.off()

  rm(fpath)
### END BOX PLOTS ###

#### ALL STATION TIME SERIES IN ONE PLOT ####
  ### Monthly TS ###
  name="output/timeseries/monthly_ts.png"
  png(filename=name, pointsize = 11, width=16, height=9, units="cm", res=300)
  matplot(m_df, type = c("l"),pch=1, lwd=2, lty=c(1), col = colors, xaxt = "n", ylab="rainfall in mm/year", main="Yearly Time Series", xlab="Year")
  axis(1,1:372,labels=substr(row.names(m_df),1,7))  
  legend(x="bottomleft", legend=stnames, col=colors, lwd=3, cex=0.8)
  dev.off()
  
  ### YEARLY TS ###
  name="output/timeseries/yearly_ts.png"
  png(filename=name, pointsize = 11, width=16, height=9, units="cm", res=300)
  matplot(y_df, type = c("b"),pch=1, lwd=2, lty=c(1), col = colors, xaxt = "n", ylab="rainfall in mm/year", main="Yearly Time Series", xlab="Year")
  axis(1,1:31,labels=substr(row.names(y_df),1,4))  
  legend(x="bottomright", legend=stnames, col=colors, lwd=3, cex=0.8)
  dev.off()
  ## Yearly raindays ##
    name="output/timeseries/yearly_raindays.png"
  png(filename=name, pointsize = 11, width=16, height=10, units="cm", res=300)
    par(def.par); par(mar=(c(2.8,2.8,0,0)+0.2));  par(cex.lab=0.7, cex.axis=0.7)
    matplot(y_raindays_df, type = c("b"),pch=1, lwd=2, lty=c(1), col = colors, xaxt = "n", ylab="rainfall in mm/year", main="Yearly Rain Days", xlab="Year")
    axis(1,1:31,labels=substr(row.names(y_df),1,4))  
    legend(x="bottomright", legend=stnames, col=colors, lwd=3, cex=0.8)
    dev.off()
### ALL STATION TIME SERIES IN ONE PLOT ###

#### CORRELATIONS ####
  fpath="output/correlation"
  dir.create(fpath)
  #### CORRELATION MATRIXES ####
  # calculate Pearson correlation between stations
  # written directly to output
  cor.matrix.write=function(x, fname){
    correlation=cor(x, use="pairwise.complete.obs", method ="pearson")
    write.csv(correlation, file=paste(fpath,fname, sep=""))
  }
  cor.matrix.write(d_df, "/daily_cor.csv")
  cor.matrix.write(w_df, "/weekly_cor.csv")
  cor.matrix.write(m_df, "/monthly_cor.csv")
  cor.matrix.write(y_df, "/yearly_cor.csv")
  
  cor.matrix.write(rs_df, "/daily_rainseason_cor.csv")
  cor.matrix.write(ds_df, "/daily_dryseason_cor.csv")
  cor.matrix.write(mrs_df, "/monthly_rainseason_cor.csv")
  cor.matrix.write(mds_df, "/monthly_dryseason_cor.csv")
  rm(cor.matrix.write)
  ###
  #### CORRELOGRAMS ####
  # -> main text
    fpath="output/correlation/correlograms"
    dir.create(fpath)

    corgr(d_df, xylim=c(0,150), type="daily", fpath=fpath)
    corgr(w_df, xylim=c(0,45), type="weekly", fpath=fpath)
    corgr(m_df, xylim=c(0,25), type="monthly", fpath=fpath)
    corgr(y_df, xylim=c(3,15), type="yearly", fpath=fpath)

    corgr(rs_df, type="rainseason daily",xylim=c(0,150), fpath=fpath)
    corgr(ds_df, type="dryseason daily",xylim=c(0,150), fpath=fpath)
    corgr(mrs_df, type="rainseason monthly",xylim=c(0,25), fpath=fpath)
    corgr(mds_df, type="dryseason monthly",xylim=c(0,25), fpath=fpath)
    rm(fpath)
  ### END CORRGRAMS ###
  #### SCATTERPLOT MATRIX ####
    fpath="output/correlation/scatterplotmatrix"
    dir.create(fpath)
    scatterMatrix(d_df, xylim=c(0,150), type="daily", fpath=fpath)
    scatterMatrix(w_df, xylim=c(0,45), type="weekly", fpath=fpath)
    scatterMatrix(m_df, xylim=c(0,25),type="monthly", fpath=fpath)
    scatterMatrix(y_df, xylim=c(3,15), type="yearly", fpath=fpath)
    
    scatterMatrix(rs_df, xylim=c(0,150),type="rainseason daily", fpath=fpath)
    scatterMatrix(ds_df, xylim=c(0,150),type="dryseason daily", fpath=fpath)
    scatterMatrix(mrs_df, xylim=c(0,25),type="rainseason monthly", fpath=fpath)
    scatterMatrix(mds_df, xylim=c(0,25),type="dryseason monthly", fpath=fpath)    
    rm(fpath)
  ### END SCATTERPLOT MATRIX ###
### END CORRELATION###

#### COMPARISON OF SPATIAL CORRELATION AND DISTANCE ####
  fpath="output/correlation/correlation_distance"
  dir.create(fpath)
# library("sp")
 library("maptools")
 library("raster")
 source("scripts/convenience_functions.R")
# Load station data (not in long lat to get distances in meters)
  stations<-readShapePoints("input/stationmap_UTM49N//stationmap-UTM49N.shp")
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
  name=paste(fpath, "/daily_corrdist.png", sep="")
  png(filename=name, pointsize = 11, width=10, height=5, units="cm", res=300)
  corrdist.plot(x=sp.dist$value,y=cor_d$value)
dev.off()
  name=paste(fpath, "/monthly_corrdist.png", sep="")
  png(filename=name, pointsize = 11, width=10, height=5, units="cm", res=300)
  corrdist.plot(x=sp.dist$value,y=cor_m$value)
dev.off()
  name=paste(fpath, "/yearly_corrdist.png", sep="")
  png(filename=name, pointsize = 11, width=10, height=5, units="cm", res=300)
  corrdist.plot(x=sp.dist$value,y=cor_y$value)
dev.off()

  rm(sp.dist.matrix, cor.matrix_d, cor.matrix_m, cor.matrix_y)
### END COMPARISON OF SPATIAL CORRELATION AND DISTANCE ###

#### shut down ####
	rm(name)
	graphics.off() #Completely shuts down the printing to file
### END SHUT DOWN ###

###### END compare.R ######
