## blob_raw.R - compiled by RoxygenReady, a package by @vertesy


#' blob 
#' 
#' blob uses the upper bound when above median, or lower bound when below
#' @template tf_template 
#' @importFrom ggplot2 aes geom_histogram geom_line ggtitle labs geom_pointrange
#'   scale_x_continuous geom_hline geom_vline xlab ylab scale_color_manual
#'   theme_classic guides ggplot
#' @importFrom dplyr mutate arrange

#' @export 

blob <- function(tf){
	numEst <- nrow(tf)
	return(tf %>% 
	         mutate(side = sign(est - median(est))
                   , pos = ifelse(side > 0, lower, upper)) %>% 
	         arrange(side
	                 , pos) %>% 
	         mutate(quantile = ((1:numEst) - 1/2)/numEst))
}


