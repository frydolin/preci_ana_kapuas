###### PRECIPITATION ANALYSIS: COMPARISON OF GROUND DATA ######

### FUNCTIONS ###

# Coerce to data frames function
mdf=function(x){
  dfr=do.call(cbind, as.data.frame(x))
  row.names(dfr)=as.character(index(x[[1]]))
  colnames(dfr)=stnames
  return(dfr)
}

corgr=function(x, type){
  require(corrgram)
  name=paste("plots/",type,"_corgr.png", sep ="")
  png(filename=name, width=800, height=800, units="px")
  corrgram(x, lower.panel=panel.pie, upper.panel=panel.pts, main=paste("Correlation between", type, "rainfall amounts"))
  dev.off()
}


