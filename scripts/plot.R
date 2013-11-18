###### PRECIPITATION ANALYSIS: COMPARISON OF GROUND DATA ######

## plot.R plots summaries to output ##

# COLOR SCHEME #
  #currently for 11
  hexcolors=c(#"#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00" ,"#CAB2D6","#6A3D9A", "#FFFF99") 
    "#FF7600",
    "#01939A",
    "#A30008",
    "#0D8800",
    "#7F054A",
    "#649305",
    "#6D14A2",
    "#17599E",
    "#0EC20E",
    "#F31D11",
    "#0B9B7F")
#### Correlograms ####
library(corrgram)
source("scripts/functions.R") #functions such as mdf,corgr,.. . As defined in the file.

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
  tsplot.pst(d_ts, type="daily")
  tsplot.pst(w_ts, type="weekly")
  tsplot.pst(m_ts, type="monthly")
  tsplot.pst(y_ts, type="yearly")

graphics.off() #Completely shuts down the printing to file
### END TS per Station ###

##### Monthly Averages #####
  name="output/plots/dav_by_month.png"
  png(filename=name, width=1000, height=700, units="px")	
  matplot(davbm_df, type = c("b"),pch=1, lty=c(1), lwd=2, col = hexcolors, xaxt = "n", ylab="rainfall in mm/day", main="Daily Average Rain per Month", xlab="Year")
  axis(1,1:12,labels=row.names(davbm_df))
  legend(x="bottomright", legend=stnames, col=hexcolors, lwd=3, cex=0.8)
  
#### Monthly TS ####
  name="output/plots/monthly_ts.png"
  png(filename=name, width=1000, height=700, units="px")
  matplot(m_df, type = c("l"),pch=1, lwd=2, lty=c(1), col = hexcolors, xaxt = "n", ylab="rainfall in mm/year", main="Yearly Time Series", xlab="Year")
  axis(1,1:372,labels=substr(row.names(m_df),1,7))  
  legend(x="bottomleft", legend=stnames, col=hexcolors, lwd=3, cex=0.8)


##### YEARLY TS ####
  name="output/plots/yearly_ts.png"
  png(filename=name, width=1000, height=700, units="px")
  matplot(y_df, type = c("b"),pch=1, lwd=2, lty=c(1), col = hexcolors, xaxt = "n", ylab="rainfall in mm/year", main="Yearly Time Series", xlab="Year")
  axis(1,1:31,labels=substr(row.names(y_df),1,4))  
  legend(x="bottomright", legend=stnames, col=hexcolors, lwd=3, cex=0.8)

#shut down
  rm(name)
  dev.off()
  graphics.off() #Completely shuts down the printing to file

### END MONTHLY AVERAGES ###


#### Cumulative Sums ####

  for (i in 1:length(cumfun_ts)) {
    name=paste("output/plots/cumfun_",stnames[i],".png", sep="")
    png(filename=name, width=800, height=500, units="px")
    plot(cumfun_ts[[i]], type="l",lty=1, lwd=2, col=hexcolors[i], ylab="rainfall in mm", main=paste("Cumulative rainfall amounts for", stnames[i]), xlab="date") 
  }

####

########## END #############
  
