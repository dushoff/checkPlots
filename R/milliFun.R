## milli_raw.R - compiled by RoxygenReady, a package by @vertesy


#' Order estimates for a millipede plot
#' 
#' The Millipede plot orders CI by the \[mean\] parameter estimate
#' 
#' 
#' @template tf_template

#' @export 

milli <-function (tf) {
	numEst <- nrow(tf)
	return(tf %>% 
	         arrange(est) %>% 
	         mutate(quantile = ((1:numEst) - 1/2)/numEst))
}


