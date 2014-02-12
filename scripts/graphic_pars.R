

## graphic_par.R sets graphic paramters

#### default
dev.off()
def.par=par(no.readonly = TRUE)

#### histograms
par(family="Lato",
    mfrow=c(5,3),
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



#### COLOR SCHEME for plots##
# repeats these 12 color values 3 times -> 36 color values
hexcolors=rep(c("#222222",
                "#0EC20E",
                "#A30008",
                "#7F054A",
                "#00FFFF",
                "#6D14A2",
                "#17599E",
                "#057E05",
                "#F31D11",
                "#0B9B7F",
                "#F36A11",
                "#FF00FF"), 3)



