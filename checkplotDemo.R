########## Michael style code to generate some example checkplots

#first call Checkplot code
source("checkFuns.R")
#binomial test
source("acceptBin.R")
#librarires
library(tidyverse)
library(furrr)
# library(exactci) #could probably get the Blaker intervals from this package

# set up parallel stuff (some tests can take too long otherwise, but could probably get rid of this and lower reps, going to change to 5e3 actually)
plan(strategy=multiprocess, workers=7)
# generate binomial data, adapted from JD notebook and using the Braker also

set.seed(7082)
numSims <- 5e4
n <- 100
#for binomial
prob <- 0.97

# for normal
mu<-22
sd<-4

## Sim
datNorm<-future_map_dfr(c(11,15,22,36), function(mymean){
    map_dfr(1:numSims, function(x){
      data.frame(mu=mymean, t(rnorm(n, mean=mymean, sd)))
      })
})

#make output with binom.test
#make again with acceptbin... this looks not good not sure we need to go further.

multBinom <- function(dat, prob0, n, testv=c("binom.test", "accept", "chisq", "wald")){
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
  #I think maybe this combines a test and an interval that doesn't use that p-value which is not the point. 
  # if(testv=="chisq"){
  #   df <- future_map_dfr(dat, function(d){
  #     bt <- prop.test(d, n, p=prob0, alternative="less")
  #     gt <- prop.test(d, n, p=prob0, alternative="greater")
  #     cp <- bt$p.value
  #     rp <- cp + runif(1)*(1-gt$p.value-cp)
  #     ci <- prop.test(d, n, p=prob0, alternative="two.sided")
  #     return(data.frame(est=d/n, cp, p=rp, upper=ci$conf.int[2], lower=ci$conf.int[1]))
  #   })
  # }
  #I think this does that properly: wald intervals and wald p-value. I think I got the wald binomial p right. 
  if(testv=="wald"){
    df <- future_map_dfr(dat, function(d){
      p <- pnorm((d/n-prob0)/(((d/n)*((n-d)/n))^0.5*n^(-0.5)))
      # gt <- prop.test(d, n, p=prob0, alternative="greater")
      # cp <- bt$p.value
      # rp <- cp + runif(1)*(1-gt$p.value-cp)
      ci <- prop.test(d, n, p=prob0, alternative="two.sided")
      return(data.frame(est=d/n, p, upper=ci$conf.int[2], lower=ci$conf.int[1]))
    })
  }
  return(df)
}

################
# make a big dataframe with different probs and 
to_checkplot<-map_dfr(c(0.5, 0.75, 0.9, 0.97), function(prob){
  dat <- rbinom(numSims, n, prob)
  k<-map_dfr(c("binom.test",  "wald"), function(testv){
   data.frame( multBinom(dat=dat, prob=prob, n=n, testv=testv), testv, prob)
    
  })
  return(k)
})

pdf("figures/wald_and_cp.pdf", height=4.5)
checkplot(to_checkplot %>% 
            mutate(testv=factor(c("Clopper-Pearson exact","Wald")[as.numeric(as.factor(testv))]
                               , levels=c("Wald","Clopper-Pearson exact")))
                   , facets=8)+
  facet_grid(testv~prob, scales="free_y")+
  labs(x="nominal p-value")+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,100))+
  theme_classic() + 
  theme(panel.spacing.x = unit(2, "lines"))
dev.off()

##########
# do it again with normal data, continuous cdf
normtests<- future_map_dfr(1:length(datNorm$mu), function(samp){
  p<-t.test(datNorm[samp, -1], mu=datNorm[samp,1], alternative="l")$p.value
  ci<-t.test(datNorm[samp, -1], mu=datNorm[samp,1])
  lower<-ci$conf.int[1]
  upper<-ci$conf.int[2]
  est<-ci$estimate
  return(data.frame(p, lower, upper, est, tm=datNorm[samp,1]))
})

######
# some tests to determine how many reps is nice for checkplots

pdf("figures/norm_22_4.pdf", width=4, height=3)
checkplot(normtests)+
  theme_classic()
dev.off()

png("figures/example_normal_slugplot_22_4.png", width=4, height=3, units="in", res=650)
rangePlot(normtests, title="slugplot: \nCIs for true mean  based on 100 deviates \nfrom norm(22,4)")
dev.off()

rangePlot(ne)+
  facet_wrap(~tm, scales="free_y")

rangePlot(ne, target = "fakemu")+
  facet_wrap(~tm, scales="free_y")


ne<-normtests %>% mutate(fakemu=tm+3)
target<-"fakemu"
ne[,target]

# print(rangePlot(testaccept, orderFun=blob, opacity=0.02))
# print(rangePlot(testchisq, orderFun=blob, opacity=0.02))
# print(rangePlot(testjd, orderFun=blob, opacity=0.02))
# print(rangePlot(testwald, orderFun=blob, opacity=0.002))
