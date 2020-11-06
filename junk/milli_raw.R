milli <- function(tf){ #milli just uses rank estimate
  numEst <- nrow(tf)
  return(tf
         %>% arrange(est)
         %>% mutate(
           quantile=((1:numEst)-1/2)/numEst
         )
  )
}