## slug_raw.R - compiled by RoxygenReady, a package by @vertesy


#' slug 
#' 
#' slug orders by the bound you care about, dynamically
#' @template tf_template 

#' @export 

slug <-function (tf) {
	numEst <- nrow(tf)
	return(tf %>% 
	         arrange(est) %>% 
	         mutate(estQ = ((1:numEst) - 1/2)/numEst
	                , pos = estQ * (lower - median(lower)) + (1 - estQ) * (upper - median(upper))
	                ) %>% 
	         arrange(pos) %>% 
	         mutate(quantile = ((1:numEst) - 1/2)/numEst))
}


