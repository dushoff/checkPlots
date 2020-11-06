blob <- function(tf){ #blob uses the upper bound when above median, or lower bound when below
  numEst <- nrow(tf)
  return(tf
         %>% mutate(
           side = sign(est-median(est))
           , pos = ifelse(side>0, lower, upper)
         )
         %>% arrange(side, pos)
         %>% mutate(
           quantile=((1:numEst)-1/2)/numEst
         )
  )
}
