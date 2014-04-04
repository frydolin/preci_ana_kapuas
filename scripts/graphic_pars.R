###### SPATIO-TEMPORAL RAINFALL PATTERNS IN KAPUAS BASIN ######
	### ANALYSIS AND COMPARISON OF GAUGE DATA ###

## graphic_pars.R
## sets graphic paramters and creates color scheme

#### DEFAULT ####
#dev.off()
par(family="Lato",
    mar=(c(4,4,3,0)+0.2),
    mgp=c(1.8,0.6,0),
    cex.axis=0.7,
    cex.lab=0.7)
def.par=par(no.readonly = TRUE)
###

#### LEGEND outside the box ####
par(xpd=TRUE,
     mar=(c(4,3,2,6.3))+0.15)
leg.out=par(no.readonly = TRUE)
par(def.par) #reset to default
###

#### COLOR SCHEME FOR PLOTS ####
# 14 colors
	colors=rainbow(n=14, s = 1, v = 0.8, start = 0.05, end = max(1, 14 - 1)/14, alpha = 1)

# As not all stations are always looked at, color values for stations excluded need to be removed in order to have a consistent coloring scheme, see also load.R 
# For statistical analysis
  colors=colors[c(-3,-8,-9,-11,-13,-14)]
# For spatial interpolation
#   colors=colors[c(-9,-13)]
###

###### END graphic_pars.R ######
