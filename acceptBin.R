#### functions from Blaker 2000 modified for R
acceptbin<-function(x, n, p){
  #Computes the acceptability of p when x is observed and X is Bin(n,p)
  
  p1<-1-pbinom(x-1,n,p)
  p2<-pbinom(x, n, p)
  a1<-p1 + pbinom(qbinom(p1, n, p) - 1, n, p) 
  a2<-p2+1-pbinom(qbinom(1-p2,n,p),n,p) 
  return(min(a1,a2))
}

acceptinterval<-function(x, n,level=0.95,tolerance=1e-04){ 
  #Computes acceptability interval for p at 1 - alpha equal to level 
  #(in (0,l)) when x is an observed value of X which is Bin(n,p). 
  lower<-0
  upper<-1
  if (x!=0){
    lower<-qbeta((1-level)/2, x, n - x + 1)
    while (acceptbin(x, n, lower) <= (1 - level)){ lower<-lower+tolerance}
    
  }
  
  
  if (x!=n){upper-qbeta(1 - (1 - level)/2, x + 1, n - x) 
    while (acceptbin(x, n, upper) <=(1 - level)) {upper<-upper-tolerance}
    
  }
  
  c(lower, upper)
  
}


#not run: test
# acceptinterval(2, 10, level=0.95)
# binom.test(2,10, 0.2)