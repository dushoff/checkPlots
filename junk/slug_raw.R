slug <- function(tf){ #slug orders by the bound you care about, dynamically
  numEst <- nrow(tf)
  return(tf
         %>% arrange(est)
         %>% mutate(
           estQ=((1:numEst)-1/2)/numEst
           , pos = estQ*(lower-median(lower)) # weight lower bound if high estimate
           +(1-estQ)*(upper-median(upper)) # weight upper bound if low estimate 
           #Weights are about equal towards the middle (not as harsh a cutoff as blob)
         )
         %>% arrange(pos)
         %>% mutate(
           quantile=((1:numEst)-1/2)/numEst
         )
  )
}
