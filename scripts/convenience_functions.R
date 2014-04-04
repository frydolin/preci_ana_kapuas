###### SPATIO-TEMPORAL RAINFALL PATTERNS IN KAPUAS BASIN ######
	### ANALYSIS AND COMPARISON OF GAUGE DATA ###

## convenience_functions.R:
## Functions for repeated tasks, that are quite specific for this analysis;
## So they are just for convenience and can hardly be recycled for other analysis
## List of functions in alphabetical order: 

#### TESTING FOR NORMALITY ####
# x: dataframe
# sign coding: (-, * and **) for non, at 5% at 1% error probability
  norm.test=function(x){
  x=lapply(x, as.ts) # test seems not to like a zoo time series only ordinary ts
  shap=lapply(x, shapiro.test)
  shap.m=sapply(shap, function(x) rbind(x$statistic,x$p.value))
  colnames(shap.m)=stnames
  shap.m=as.data.frame(shap.m)
  sign=ifelse(shap.m[2,]<0.05, "*", "-")
  sign=ifelse(shap.m[2,]<0.01, "**",sign)
  shap.m=rbind(shap.m, sign)  
  rownames(shap.m)=c("statistic", "p.value", "significance")
  shap.m=t(shap.m) 
  return(shap.m)
  }
### 

#### homogeneity.tests ####
# for n=30
# returns a vector of significance 
# (-, * and **) for non, at 5% at 1% error probability

homogeneity.tests=function(x){
    source("scripts/homogeneity_tests.R")
    #testing
    neumann<-lapply(x, neumann.ratio, na.rm=TRUE)
    buishand<-lapply(x, buishand.test, na.rm=TRUE)
    pettitt<-lapply(x, pettitt.test, na.rm=TRUE)
    snht<-lapply(x, snh.test, na.rm=TRUE)
    # extract and check for significance
    #sign vlues should be reckecked
    nm=sapply(neumann, function(x) x$N)
    nm_s=ifelse(nm<1.4, "*", "-")
    nm_s=ifelse(nm<1.2, "**", nm_s)
    bs=sapply(buishand, function(x) x$R.sign)
    bs_s=ifelse(bs>1.5, "*", "-")
    bs_s=ifelse(bs>1.7, "**", bs_s)
    pt=sapply(pettitt, function(x) x$X_e)
    pt_s=ifelse(pt>107, "*", "-")
    pt_s=ifelse(pt>133, "**", pt_s)
    sn=sapply(snht, function(x) x$T_0)
    sn_s=ifelse(sn>7.65, "*", "-")
    sn_s=ifelse(sn>10.45, "**", sn_s)
    
    #return list of significance
    return(list("nm"=nm_s, "bs"=bs_s, "pt"=pt_s, "sn"=sn_s ))
}
###

#### mk.trendtest ####
# Mann-Kendall-Trend testing 
# returns a data frame with Kendall'stau and significance values (p value and coded with -,*,**)
# tau=Score/denominator, denominator=max possible value for score
## x: list of zoo objects to be tested
## test: type of test (SeasonalMannKendall or MannKendall)
  mk.trendtest=function(x, test){
  require("Kendall")
  trendtest=lapply(x, function(x) test(as.ts(x)))
  # Extract values:
  tau=sapply(trendtest, function(x) x$tau)
  sl=sapply(trendtest, function(x) x$sl) #sl=two-sided p-value
  # Code p-values with * and **
  sign=ifelse(sl<0.05, "*", "")
  sign=ifelse(sl<0.01, "**",sign)
  tau=round(tau, digits = 5) #round tau to 3  digits
  sl=round(sl, digits = 5) #round sl to 3  digits
  p.sl=paste(sl, sign, sep="")
  return(as.data.frame(cbind(tau, p.sl)))
  }
###

#### reshape.matrix ####
# Reshaping of distance and correlation matrixes
## x: matrix
reshape.matrix=function(x){
	require("reshape")
  x=x[order(rownames(x)),order(colnames(x))] #reorder
  x[upper.tri(x, diag=FALSE)]=NA # set redundant entries to NA
  y= melt(x) #melt
  y=y[!is.na(y$value),] #remove NA entries (they are duplicates of the quadratic matrix)
  y=y[with(y, order(X1, X2)), ] #reorder
  row.names(y)=paste(y$X1,"-",y$X2,sep="")
  return(y)
}
###

#### tsplot.pst ####
# Make time series plots for each station (time series plot per station)
## x: list of zoo time series objects
## type: is for naming e.g. daily, monthly. EXCEPTION: "yearly" also changes plot type to "b"!
## fpath: file path
## colors and graphical paramteres need to be defined in the graphic_pars.r file
tsplot.pst=function(x, type, fpath, ...) {
  require("hydroTSM")
  source("scripts//graphic_pars.R")
  # make directory, depending on type:
  npath=paste(fpath,"/",type, sep="")
  dir.create(npath)
  # if it is yearly time series lines and points are plotted, else only lines:
  if(type=="yearly") ptype="b" else ptype="l" 
  # make graphs:
  for (i in 1:length(x)) {
    name=paste(npath,"/",type,"_ts_",stnames[i],".png", sep="")
    png(filename=name, pointsize = 11, width=16, height=5, units="cm", res=300)  
    par(def.par); par(mar=(c(2,2.8,0,0)+0.2));  par(cex.lab=0.7, cex.axis=0.7, mgp=c(2.1,0.6,0))
    plot(x[[i]], type=ptype, lty=1, lwd=1, las=1, col=colors[i], ylab="rainfall (mm/day)", xlab="", xaxt="n", ...)
    drawTimeAxis(dummy, tick.tstep = "years", lab.tstep = "years", lab.fmt="%Y", cex=0.7)
    dev.off()
  }
}
###

#### double.ts ####
# Plots time series of rainy days and rainfall on top of each other (a double time series)
## x: list of zoo time series of rainfall amounts 
## y: list of zoo time series of number of rainy days
## fpath: outpur file path
double.ts=function(x, y, fpath){
  for (i in 1:length(x)) {
    name=paste(fpath,"/","double_ts_",stnames[i],".png", sep="")
    png(filename=name, pointsize = 11, width=16, height=7.5, units="cm", res=300)
    par(def.par); par(mar=c(4, 4, 0, 5) + 0.2)
    ## Plot rainfall amounts plot and put axis scale on right side:
    plot(x[[i]], xlab="", ylab="", xaxt = "n", yaxt = "n", type="b", col=colors[i], pch=19, cex=0.8, lty=1)
    axis(4,col=colors[i],las=1)
    mtext("rainfall (mm/day)",side=4,col=colors[i],line=3)
    ## Plot rainday data and draw its axis:
    par(new=TRUE)
    ymid=mean(y[[i]], na.rm=TRUE) # mean in the middle of the axis
    plot(y[[i]], ylim=c(ymid-50,ymid+50),xaxt = "n", yaxt = "n", xlab="", ylab="", type="b", col="black", cex=0.8, lty=3)
    axis(2, ylim=c(ymid-50,ymid+50),col="black",las=1) 
    mtext("No. of raindays",side=2,line=2.5)
    ## Draw the time axis:
    axis(1,at=time(y_ts[[1]]), labels=format.Date(time(y_ts[[1]]), "%Y"))
    box()
    dev.off()
  }
}
###

#### cuml.plot ####
# Plots empirical cumulative distribution functions in one plot
## x: list of ecdf objects, as created by stats::ecdf
## fname: filename
cuml.plot=function(x, fname){
  png(filename=name, pointsize = 11, width=16, height=10, units="cm", res=150)
  par(def.par); par(mar=(c(3,3,0.8,0)+0.2)); par(cex.lab=0.7, cex.axis=0.7)
  	# Plot first one
      plot(x[[1]], do.points=FALSE, verticals=TRUE, col.01line = "black", col=colors[1],  xlab="rainfall (mm/day)", main="")          
      for (i in 2:length(x)){ 
        lines(x[[i]], do.points=FALSE, verticals=TRUE, col=colors[i], lty=1)
      }
      abline(0.5,0, lty=2)
      legend(x="bottomright", inset=c(0,0.05), legend=stnames, lty=1, col=colors, lwd=2, cex=0.7, bty="n")
  dev.off()
}
###

#### hist.kde.plot ####
# Comparative plots of histogramms with kernel density estimate overlaid.
# Arranged in one single figure in 3*x layout.
## hist.x: data to be used to calculate histogramms from
## onlyraindays: TRUE or FALSE, if TRUE only days with rainfall>1mm are considered
## h: bin width of histogramms
## kde.x: Kernel density estimate as created by stats::density
## rug: plot a rug plot, TRUE or FALSE
## ...: further parameters passed on
hist.kde.plot=function(hist.x, onlyraindays=FALSE, h, kde.x, rug=TRUE, ...){
  require("MASS")
  source("scripts//graphic_pars.R")
  # set parameters:
  par(def.par); par(mfrow=c(round(length(hist.x)/3),3)); par(cex.axis=1.1, cex.lab=1.1); par(mar=c(0.5,0,1,0.4), oma=c(3.5,4,0,0), adj=0, las=1, lwd=1); par(mgp=c(1.5,0.5,0))
  for (i in 1:length(hist.x)){
    # axis should only plotted on the outside plots:
    if (i %in% seq(1, length(hist.x),3)){yax="s"} else {yax="n"} # axes on left side
    if (i %in% c((length(hist.x)-2):length(hist.x))){xax="s"} else {xax="n"} # axes on the bottom
    # plot histogramm:
    if (onlyraindays==TRUE){hist.x[[i]]=hist.x[[i]][which(hist.x[[i]]>=1)]} #selects only raindays
    truehist(hist.x[[i]], prob=TRUE, h=h,col="#eeeeee", main=paste(stnames[i]), yaxt=yax, xaxt=xax, ...)
    # plot rug plot:
    if (rug==TRUE) {rug(hist.x[[i]], ticksize=0.1, lwd=0.5, line=0)}
    # add KDE plot:
    lines(kde.x[[i]], lwd=1.5, col="black")
    box(which="plot")
  }
  mtext("Frequency", side = 2, line = 2.8, cex=0.8, las=0, outer = TRUE, at = NA,  adj = 0.5, padj = 0.5)
  mtext("rainfall (mm/day)", side = 1, line =1, cex=0.8, outer = TRUE, adj = 0.5, padj = 0.5)
}
###

#### bplot.bswarm ####
# Create a boxplot with overlaid beeswarm plot
## ts: list(?????????) of zoo time series objects
## df: same data as ts in one single data frame
## xlabel: TRUE or FALSE (default: TRUE)
## ...: further arguments passed on
bplot.bswarm=function(ts, df, xlabel=TRUE, ...){
  require("beeswarm")
  if (xlabel==TRUE) {par(mar=(c(2.6,3.4,0,0)+0.2))} else par(mar=(c(0.8,3.4,1.8,0)+0.2))  
  par(mgp=c(2.7, 0.6, 0), las=1)
  boxplot(df, outline=FALSE, xaxt="n", ...)
  beeswarm(ts, pch=21, bg=colors, cex=0.7, add=TRUE)
  abline(mean(df,  na.rm=TRUE),0, lwd=2, lty=2, col="#dd4444")
  axis(1, at=(1:length(ts)), labels = FALSE)
  if (xlabel==TRUE) {text(1:length(ts), par("usr")[3]*0.65, srt = 35, adj = 1, labels = colnames(df), xpd = TRUE, cex=0.7)}
}
###

#### corrdist.plot ####
# Makes plots comparing correlation values with distance
## x: distance values
## y:	correlation values
corrdist.plot=function(x,y){
  par(def.par); par(cex.lab=0.8, cex.axis=0.7, las=1); par(mar=(c(2.8,2.8,0,0)+0.2))
  plot(y~x, ylim=c(0,1.1),  xlim=c(0,280), lty=2, xlab="distance (km)", ylab="r (Pearson)")
}
###

###### END convenience_functions.R ######
