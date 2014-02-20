###### SPATIO-TEMPORAL RAINFALL PATTERNS IN KAPUAS BASIN ######
### Analysis and comparison of station data ###

## graphic_par.R sets graphic paramters

#### default ####
#dev.off()
par(family="Lato",
    mar=(c(4,4,3,0)+0.2))
def.par=par(no.readonly = TRUE)

#### histograms ####
par(mfrow=c(5,3),
    adj=0,
    mar=(c(2,2.4,1,0)+0.1),
    col="black",
    mgp=c(3,0.5,0),
    oma=c(2,2,0,0),
    lwd=1,
    las=1,
    cex.axis=0.7)
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
  library("colorspace")
  set.seed(1)
  colors=rainbow_hcl(14, c=sample(seq(50, 100, length.out =14)), l=seq(30, 70, length.out =14), start=420, end = 60, fixup=TRUE)
# pal((colors))
#    pal(desaturate(colors))
#   set.seed(70)
#   colors=sample(colors)
###
### END graphic_pars.R ###