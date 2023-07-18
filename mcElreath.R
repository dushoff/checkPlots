# https://gist.githubusercontent.com/rmcelreath/4f7010e8d5688c69bbeb7008f0aabe65/raw/3bfde2a7d162038bf54856b649b4c268573fc061/p_under_null.r
# distribution of null p-values in binomial glm

library(rethinking)

# t test
# pval has uniform distribution under null

f <- function(N=10) {
    y1 <- rnorm(N)
    y2 <- rnorm(N)
    z <- t.test(y1,y2)
    return( z$p.value )
}

S <- 1e5
pvals1 <- mcreplicate(S,f(N=100),mc.cores=8)
hist(pvals1,main="null p-values for t.test",breaks=50,border="white",col=4,xlab="p value")
sum(pvals1 < 0.05 ) / S

# binomial
# pval DOES NOT in general have uniform distribution under null

f2 <- function(N=10,M=5,p=0.25,b=0) {
    x <- c( rep(0,N/2) , rep(1,N/2) )
    p <- inv_logit(logit(p)+b*x)
    y <- rbinom(N,size=M,prob=p)
    z <- glm( cbind(y,M-y) ~ x , family=binomial )
    pval <- summary(z)$coefficients[,4][2]
    return( as.numeric(pval) )
}

S <- 1e5
pvals <- mcreplicate(S,f2(N=100,M=1,p=0.85,b=0),mc.cores=8)
hist(pvals,main="null p-values for binomial",breaks=50,border="white",col=4,xlab="p value")
sum(pvals < 0.05 ) / S
