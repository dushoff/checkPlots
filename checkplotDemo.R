########## Michael style code to generate some example checkplots

#first call Checkplot code
source("checkFuns.R")
#binomial test
source("acceptBin.R")
#librarires
library(tidyverse)
library(furrr)
library(exactci)
binom.exact
# set up parallel stuff (take too long otherwise)
plan(strategy=multiprocess, workers=7)
# generate binomial data, adapted from JD notebook and using the Braker also

set.seed(7082)
numSims <- 1e5
n <- 100
prob <- 0.97
mu<-22
sd<-4

## Sim
dat <- rbinom(numSims, n, prob)

#make output with binom.test
#make again with acceptbin... this looks not good not sure we need to go further.

multBinom <- function(dat, prob0, n, testv=c("binom.test", "accept", "chisq")){
  if(testv=="binom.test"){
    df <- map_dfr(dat, function(d){
    # df<-future_map_dfr(1:length(dat)/100, function(repset){
    #   map_dfr(1:100, function(nexti){
    #     d <- dat[(repset-1)*100+nexti]
        bt <- binom.test(d, n, p=prob0, alternative="less")
        gt <- binom.test(d, n, p=prob0, alternative="greater")
        cp <- bt$p.value
        rp <- cp + runif(1)*(1-gt$p.value-cp)
        ci <- binom.test(d, n, p=prob0, alternative="two.sided")
        return(data.frame(est=d/n, cp, p=rp, upper=ci$conf.int[2], lower=ci$conf.int[1]))
      })
    # })
  }
  if(testv=="accept"){
    df <- future_map_dfr(dat, function(d){
      p <- acceptbin(d, n, p=prob0)
      ci <- acceptinterval(d, n)
      return(data.frame(est=d/n, p, upper=ci[2], lower=ci[1]))
    })
  }
  if(testv=="chisq"){
    df <- future_map_dfr(dat, function(d){
      bt <- prop.test(d, n, p=prob0, alternative="less")
      gt <- prop.test(d, n, p=prob0, alternative="greater")
      cp <- bt$p.value
      rp <- cp + runif(1)*(1-gt$p.value-cp)
      ci <- prop.test(d, n, p=prob0, alternative="two.sided")
      return(data.frame(est=d/n, cp, p=rp, upper=ci$conf.int[2], lower=ci$conf.int[1]))
    })
  }
  return(df)
}

################
# make a big dataframe with different probs and 
to_checkplot<-map_dfr(c(0.75, 0.9, 0.97), function(prob){
  dat <- rbinom(numSims, n, prob)
  k<-map_dfr(c("binom.test",  "chisq"), function(testv){
   data.frame( multBinom(dat=dat, prob=prob, n=n, testv=testv), testv, prob)
    
  })
  return(k)
})

checkplot(to_checkplot, facets=6)+
  facet_grid(testv~prob, scales="free_y")+
  labs(x="nominal p-value")+
  scale_x_continuous(expand=c(0,0))+
  theme_classic()

##########
# do it again with normal data, continuous cdf


testaccept<-multBinom(dat, prob, n, testv="accept")
testchisq<-multBinom(dat, prob, n, testv="chisq")
testjd<-multBinom(dat, prob, n, testv="binom.test")

print(pianoPlot(testaccept$p))
print(pianoPlot(testchisq$p))
print(pianoPlot(testjd$p))

print(rangePlot(testaccept, orderFun=blob, opacity=0.02))
print(rangePlot(testchisq, orderFun=blob, opacity=0.02))
print(rangePlot(testjd, orderFun=blob, opacity=0.02))
