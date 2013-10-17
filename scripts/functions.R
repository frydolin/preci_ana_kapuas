###### PRECIPITATION ANALYSIS: COMPARISON OF GROUND DATA ######

## functions.R contains own functions for the analysis ##

# Coerce zoo time series to data frames function: mdf (make data frame)#
## x: list of zoo time series objects
mdf=function(x){
  dfr=do.call(cbind, as.data.frame(x))		# converison
  row.names(dfr)=as.character(index(x[[1]])) 	# only naming
  colnames(dfr)=stnames				# only naming
  return(dfr)
}

# own version of correlograms: corgr. creates *.png files in output/plots/ # 
## x: should be a data matrix (as in the normal corrgram() function)
## type: is only for naming e.g. daily, monthly 
corgr=function(x, type){
  require(corrgram)
  name=paste("output/plots/",type,"_corgr.png", sep ="")	# filename
  png(filename=name, width=800, height=800, units="px")		# open *.png write
  corrgram(x, lower.panel=panel.pie, upper.panel=panel.pts, main=paste("Correlation between", type, "rainfall amounts"))
  dev.off()							# close write
}


