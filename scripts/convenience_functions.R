###### SPATIO-TEMPORAL RAINFALL PATTERNS IN KAPUAS BASIN ######
### Analysis and comparison of station data ###

## convenience_functions.R: functions for repeated tasks, that are very specific
## for this analysis so they are just for convenience and can't be recycled
## for other purposes

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

#### Mann-Kendall-Trend Testing ####
  ## tau=Score/denominator, denominator=max possible value for score
  # x: list of zoo objects
  # test: type of test (SeasonalMannKendall, or MannKendall)
  # returns kendall's tau and significance (p value coded with -,*,**)
  mk.trendtest=function(x, test){
  require("Kendall")
  trendtest=lapply(x, function(x) test(as.ts(x)))
  tau=sapply(trendtest, function(x) x$tau)
  sl=sapply(trendtest, function(x) x$sl) #sl:  two-sided p-value
  sign=ifelse(sl<0.05, "*", "")
  sign=ifelse(sl<0.01, "**",sign)
  tau=round(tau, digits = 5) #round tau to 3  digits
  sl=round(sl, digits = 5) #round sl to 3  digits
  p.sl=paste(sl,sign, sep="")
  return(as.data.frame(cbind(tau, p.sl)))
  }

#### Reshaping of distance and correlation matrixes: ####
reshape.matrix=function(x){
  x=x[order(rownames(x)),order(colnames(x))]
  x[upper.tri(x, diag=FALSE)]=NA
  y= melt(x) #melt
  y=y[!is.na(y$value),] #remove NA entries (they are duplicates of the quadratic matrix)
  y=y[with(y, order(X1, X2)), ] #reorder
  row.names(y)=paste(y$X1,"-",y$X2,sep="")
  return(y)
}
###

#### CUMULATIVE PLOTS ####
# x: list of ecdf objects
cuml.plot=function(x){
  png(filename=name, width=500, height=300, units="px")
  par(def.par); par(mar=(c(3,3,0.8,0)+0.2)); par(cex.lab=0.7, cex.axis=0.7)
      plot(x[[1]], do.points=FALSE, verticals=TRUE, col.01line = "black", col=colors[1], xlab="rainfall (mm/day)", main="")          
      for (i in 2:length(x)){ 
        lines(x[[i]], do.points=FALSE, verticals=TRUE, col=colors[i], lty=1)
      }
      abline(0.5,0, lty=2)
      legend(x="bottomright", inset=c(0,0.05), legend=stnames, lty=1, col=colors, lwd=2, cex=0.7, bty="n")
  dev.off()
}
##### END convenience_functions #####