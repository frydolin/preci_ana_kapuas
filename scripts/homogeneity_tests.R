#### HOMOGENEITY TESTS for rainfall time series #### 

##According to Wijngaard, J. B., Klein Tank, A. M. G., & K�nnen, G. P. (2003).
##Homogeneity of 20th century European daily temperature and precipitation
##series. International Journal of Climatology, 23(6), 679–692.
##doi:10.1002/joc.906


neumann.ratio=function(x, na.rm=TRUE){
  require("zoo")
  #Check input
  if (!is.zoo(x)&&!is.vector(x)) stop("Invalid argument: 'class(x)' must be in c('vector' or 'zoo')")
  #Convert to vector of zoo objects, because rank() needs that
  if (is.zoo(x)){x=as.vector(x)}
  
  n    <-sum(!is.na(x)) 
  x_mean= (mean(x, na.rm=na.rm))
  ind.length= length(x)
  x.plusone= x[2:ind.length]
  x.minuslast= x[1:(ind.length-1)]
  N=(sum((x.minuslast-x.plusone)^2, na.rm=na.rm)/sum((x-x_mean)^2, na.rm=na.rm))
  return(list("n"=n, "Neumann.ratio"=N))
}

buishand.test=function(x, na.rm=TRUE){
   x_mean=mean(x, na.rm=na.rm)
   n    <-sum(!is.na(x)) 
   dif=x-x_mean
   Sk=numeric()
   Sk[0]=0
   for(k in (1:n)){Sk[k]=sum(dif[1:k], na.rm=na.rm)}
   rSk=(Sk/sd(x, na.rm=na.rm)) #rescaled adjusted partial sums
   R=max(rSk, na.rm=na.rm)-min(rSk, na.rm=na.rm)
   Q=max(abs(rSk), na.rm=na.rm)
   R.sign=R/(sqrt(n))
   Q.sign=Q/(sqrt(n))
   break.points=which(abs(rSk)==Q)
   if (R.sign==0){R.sign=NA}
   return(list("n"=n,"rSk"=rSk, "abs.Max"=Q, "R.sign"=R.sign, "breakpoints"=break.points))
}

pettitt.test=function(x, na.rm=TRUE){
  require("zoo")
  #Check input
  if (!is.zoo(x)&&!is.vector(x)) stop("Invalid argument: 'class(x)' must be in c('vector' or 'zoo')")
  #Convert to vector of zoo objects, because rank() needs that
  if (is.zoo(x)){x=as.vector(x)}
  n       <-sum(!is.na(x))
  x.rank=rank(x, ties.method="average", na.last=na.rm)
  Xk=numeric()
  for(k in (1:n)){
    sum.part=numeric()
    for(i in (1:k)){sum.part[i]=x.rank[i]}
  Xk[k]=2*(sum(sum.part, na.rm=na.rm))-k*(n+1)
    rm(sum.part)
  }  
  Xe=max(abs(Xk))
  position=which(abs(Xk)==Xe)
  return(list("n"=n, "X_k"=Xk, "X_e"=Xe, "breakpoints"=position))
}

snh.test=function(x, na.rm=TRUE){
  n       <-sum(!is.na(x))  #missing values consequently removed
  mean_x  <-mean(x, na.rm=na.rm)
  sd_x    <-sd(x, na.rm=na.rm)
  dif  <-(x-mean_x)/sd_x #Vector of differences between value and mean, scaled by sd
  z_1=numeric()
  z_2=numeric()
  T_k=numeric()
  for (k in 1:n){
  z_1[k]=(1/k)*sum(dif[1:k], na.rm=na.rm) #sum up the vector
  z_2[k]=(1/(n-k))*sum(dif[(k+1):n],  na.rm=na.rm)
  T_k[k] <- k*(z_1[k])^2+(n-k)*(z_2[k])^2 # k=1...n
  }
  T_0=max(T_k, na.rm=na.rm)
  if (T_0==0){T_0=NA} #set to NA 
  return(list("n"=n, "T_k"=T_k, "T_0"=T_0))
}

### END ###