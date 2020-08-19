library(boot)
library(tidyverse)

n<-50 #sample size
p<-0.1 #probability of success

set.seed(1615) #set random seed for replicability



my_p<-function(x, indices){sum(x[indices])/length(x[indices])} #define test statistic for boot


test_coverage<-map_dfr(1:4000, function(x){     # repeatedly bootstrap to determine empirical coverage
  empirical<-rbinom(n, 1, p) #take empirical sample
  ci<-boot.ci(boot(data = empirical, statistic = my_p, R = n)
        , conf = 0.95
        , type = "basic")
  return(data.frame(lwr=ci$basic[4], upr=ci$basic[5]))
})

outside<- function(p, ci){ #fraction of samples for which true parameter falls outside CI
  (sum(p<ci$lwr)+sum(p>ci$upr))/length(ci$lwr)
}
cov_obs<-1-outside(p, test_coverage)

cov_obs # 0.86, should be 0.95

