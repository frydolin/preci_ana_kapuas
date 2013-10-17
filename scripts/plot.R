###### PRECIPITATION ANALYSIS: COMPARISON OF GROUND DATA ######

## plot.R plots summaries to output ##

#### Correlograms ####
library(corrgram)
source("scripts/functions.R") #functions such as mdf,corgr,.. . As defined in the file.

corgr(df_gdata, type="daily")
corgr(w_gdata, type="weekly")
corgr(m_gdata, type="monthly")
corgr(y_gdata, type="yearly")

corgr(rs_gdata, type="rainseason daily")
corgr(ds_gdata, type="dryseason daily")
corgr(mrs_gdata, type="rainseason monthly")
corgr(mds_gdata, type="dryseason monthly")


graphics.off() #Completely shuts down the printing to file

### END CORRGRAMS ###

##### Monthly Averages #####
  name="/output/plots/dav_by_month"
  png(filename=name, width=1000, height=700, units="px")	
  plot(df_mav[,1], type="b", lwd=2,col=1, ylim=c(0,max(df_mav)), xaxt = "n", ylab="Daily Average Rain per Month", xlab="Month")
  axis(1,1:12,labels=row.names(df_mav))
  for (i in 2:ncol(df_mav)) lines(df_mav[,i], type="b", col=i, lwd=2)
  legend(x="bottomright", legend=stnames, col=1:ncol(df_mav), lwd=3, cex=0.8)
  
  #shut down
  rm(name)
  dev.off()
  graphics.off() #Completely shuts down the printing to file

### END MONTHLY AVERAGES ###

########## END #############
  
