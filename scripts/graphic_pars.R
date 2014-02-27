###### SPATIO-TEMPORAL RAINFALL PATTERNS IN KAPUAS BASIN ######
### Analysis and comparison of station data ###

## graphic_par.R sets graphic paramters

#### default ####
#dev.off()
par(family="Lato",
    mar=(c(4,4,3,0)+0.2),
    mgp=c(1.8,0.6,0),
    cex.axis=0.7,
    cex.lab=0.7)
def.par=par(no.readonly = TRUE)

#### histograms ####
par(par(mfrow=c(round(length(d_ts)/3),3)), #a*b=length(x)    
    adj=0,
    mar=(c(2,2.4,1,0)+0.1),
    col="black",
    mgp=c(3,0.5,0),
    oma=c(2,2,0,0),
    lwd=1,
    las=1
    )
hist.par=par(no.readonly = TRUE)
par(def.par)

#### legend outside ####
par(xpd=TRUE,
     mar=(c(4,3,2,6.3))+0.15)
leg.out=par(no.readonly = TRUE)
par(def.par)
##
#### COLOR SCHEME for plots ####
# For 14 colors
colors=rainbow(n=14, s = 1, v = 0.8, start = 0.05, end = max(1, 14 - 1)/14, alpha = 1)
#   pal((colors))
#   pal((desaturate(colors)))

# For statistical analysis
  colors=colors[c(-3,-8,-9,-11,-13,-14)]
# For spatial interpolation
#   colors=colors[c(-9,-13)]
###
### END graphic_pars.R ###