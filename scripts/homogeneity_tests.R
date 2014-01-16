
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
   n=length(x)
   dif=x-x_mean
   Sk=numeric()
   Sk[0]=0
   for(k in (1:n)){Sk[k]=sum(dif[1:k], na.rm=TRUE)}
   rSk=(Sk/sd(x, na.rm=TRUE)) #rescaled adjusted partial sums
   R=max(rSk, na.rm=TRUE)-min(rSk, na.rm=TRUE)
   Q=max(rSk, na.rm=TRUE)
   R.sign=R/(sqrt(n))
   Q.sign=Q/(sqrt(n))
   return(list("Sk"=Sk,"rSk"=rSk, "sd"=sd(Sk), "Qsign"=Q.sign, "Rsignificance"=R.sign, length(x)))
}

pettitt.test=function(x){
  x.rank=rank(x, ties.method="average", na.last=TRUE)
  Xk=numeric()
  for(k in (1:length(x))){
    sum.part=numeric()
    for(i in (1:k)){sum.part[i]=x.rank[i]}
  Xk[k]=2*(sum(sum.part))-k*(length(x)+1)
  }
  rm(sum.part)
  Xe=max(abs(Xk))
  position=which(abs(Xk)==Xe)
  return(list("Xk"=Xk, "max"=Xe, "break.pos"=position ))
}
