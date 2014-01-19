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
  
  n    <-sum(!is.na(x)) #just for output illustration
  x_mean= (mean(x, na.rm=na.rm))
  ind.length= length(x)
  x.plusone= x[2:ind.length]
  x.minuslast= x[1:(ind.length-1)]
  N=(sum((x.minuslast-x.plusone)^2, na.rm=na.rm)/sum((x-x_mean)^2, na.rm=na.rm))
  return(list("n"=n, "Neumann.ratio"=N))
}

buishand.test=function(x, na.rm=TRUE){
   n    <-length(x)       #index length
   nn   <-sum(!is.na(x))  #number of valid values
   if (na.rm==FALSE && n>nn ) {stop("Data contains missing values, 
                             set na.rm to TRUE if you want to use the function anyway")}
   x_mean=mean(x, na.rm=na.rm)
   dif=x-x_mean
   Sk=numeric()
   Sk[0]=0
   for(k in (1:n)){Sk[k]=sum(dif[1:k], na.rm=na.rm)}  #sum up over whole index
   rSk=(Sk/sd(x, na.rm=na.rm)) #rescaled adjusted partial sums
   R=max(rSk, na.rm=na.rm)-min(rSk, na.rm=na.rm)
   Q=max(abs(rSk), na.rm=na.rm)
   R.sign=R/(sqrt(nn))
   Q.sign=Q/(sqrt(nn))
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
  
  n    <-length(x)       #index length
  nn   <-sum(!is.na(x))  #number of valid values
  x.rank=rank(x, ties.method="average", na.last=NA)
  Xk=numeric()
  for(k in (1:n)){
    sum.part=x.rank[1:k]
    Xk[k]=2*(sum(sum.part))-k*(nn+1)
    }  
  Xe=max(abs(Xk))
  position=which(abs(Xk)==Xe)
  return(list("n"=n, "X_k"=Xk, "X_e"=Xe, "breakpoints"=position))
}

snh.test=function(x, na.rm=TRUE){
  if(na.rm==TRUE){x=x[!is.na(x)]} #remove NAs, because of scaling with n, this is the best way?!
  n    <-length(x)       #index length
  mean_x  <-mean(x)
  sd_x    <-sd(x)
  dif  <-(x-mean_x)/sd_x #Vector of differences between value and mean, scaled by sd
  z_1=numeric()
  z_2=numeric()
  T_k=numeric()
  for (k in 1:n){
  z_1[k]=(1/k)*sum(dif[1:k]) #sum up the vector
  z_2[k]=(1/(n-k))*sum(dif[(k+1):n])
  T_k[k] <- k*(z_1[k])^2+(n-k)*(z_2[k])^2 # k=1...n
  }
  T_0=max(abs(T_k), na.rm=TRUE)
  if (T_0==0){T_0=NA} #set to NA 
  return(list("n"=n, "T_k"=T_k, "T_0"=T_0))
}

### END ###