
neumann.ratio=function(x){
  x_mean= (mean(x, na.rm=TRUE))
  ind.length= length(x)
  x.plusone= x[2:ind.length]
  x.minuslast= x[1:(ind.length-1)]
  N=(sum((x.minuslast-x.plusone)^2, na.rm=TRUE)/sum((x-x_mean)^2, na.rm=TRUE))
  return(list("Neumann.ratio"=N))
}

buishand.test=function(x){
   x_mean=mean(x, na.rm=TRUE)
   n    <-sum(!is.na(x)) 
   dif=x-x_mean
   Sk=numeric()
   Sk[0]=0
   for(k in (1:n)){Sk[k]=sum(dif[1:k], na.rm=TRUE)}
   rSk=(Sk/sd(x, na.rm=TRUE)) #rescaled adjusted partial sums
   R=max(rSk, na.rm=TRUE)-min(rSk, na.rm=TRUE)
   Q=max(abs(rSk), na.rm=TRUE)
   R.sign=R/(sqrt(n))
   Q.sign=Q/(sqrt(n))
   break.point=which(abs(rSk)==Q)
   return(list("Sk"=Sk,"rSk"=rSk, "sd"=sd(Sk), "abs.Max"=Q, "Rsignificance"=R.sign, "breakpoint"=break.point))
}

pettitt.test=function(x){
  n       <-sum(!is.na(x))
  x.rank=rank(x, ties.method="average", na.last=TRUE)
  Xk=numeric()
  for(k in (1:n)){
    sum.part=numeric()
    for(i in (1:k)){sum.part[i]=x.rank[i]}
  Xk[k]=2*(sum(sum.part, na.rm=TRUE))-k*(n+1)
    rm(sum.part)
  }  
  Xe=max(abs(Xk))
  position=which(abs(Xk)==Xe)
  return(list("X_k"=Xk, "X_e"=Xe, "break.pos"=position))
}

snh.test=function(x, na.rm=TRUE){
  n       <-sum(!is.na(x))  #missing values consequently removed
  mean_x  <-mean(x, na.rm=TRUE)
  sd_x    <-sd(x, na.rm=TRUE)
  dif  <-(x-mean_x)/sd_x #Vector of differences between value and mean, scaled by sd
  z_1=numeric()
  z_2=numeric()
  T_k=numeric()
  for (k in 1:(n-1)){
  z_1[k]=(1/k)*sum(dif[1:k], na.rm=TRUE) #sum up the vector
  z_2[k]=(1/(n-k))*sum(dif[(k+1):n])
  T_k[k] <- k*(z_1[k])^2+ (n-k)*(z_2[k])^2 # k=1...(n-1), for k=n there would be division by zero
  }
  T_0=max(T_k, na.rm=na.rm)
  return(list("T_k"=T_k, "T_0"=T_0))
}