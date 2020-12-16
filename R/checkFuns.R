#' Create a checkplot
#' 
#' Visual tool to determine if nominal p-values for Monte-Carlo simulations
#' under a given (null) hypothesis is true have uniform distribution
#' 
#' @importFrom ggplot2 aes geom_histogram geom_line ggtitle labs geom_pointrange
#'   scale_x_continuous geom_hline geom_vline xlab ylab scale_color_manual
#'   theme_classic guides ggplot
#' 
#' @importFrom dplyr mutate arrange
#' 
#' @param stats Dataframe with a column called `p` containing nominal p-values
#' @param breaks Numeric vector in \[0,1\], determining bar widths for
#'   geom_histogram
#' @param tag optional character string for a ggtitle
#' @param Wmin scalar, to filter the df `stats`
#' @param facets Integer, number of facets if using facet_wrap or similar
#' 
#' @return ggplot object, a "checkPlot"
#' @aliases checkplot
#' 
#' @export
checkPlot <- function(stats, breaks = seq(0,1,0.05), tag = "", Wmin = 0, facets = 1){
	# stats <- filter(stats, W>Wmin)
	return(ggplot(stats, aes(p))
		+ geom_histogram(breaks = breaks)
		+ geom_hline(yintercept = nrow(stats)/(facets * (length(breaks) - 1)))
		+ ggtitle(tag)
		+ labs(x = "nominal p-value")
	)
}
#' Create Rangeplot Based On Order Function
#' 
#' Confidence intervals (CI) consisting of an estimate, upper, and lower limit
#' from Monte-Carlo sampling (given that the statistical hypothesis is true) are
#' arranged according to \code{orderFun} and compared with the hypothesized
#' (true) value of the statistic to examine statistical coverage and bias of CI
#' 
#'  @template tf_template
#'  @param target scalar, value of statistic under (true) hypothesis
#'  @param orderFun one of \code{c(slug, milli, blob)}, determining the way CI
#'  are arranged in the plot
#'  @param conf scalar in [0,1], nominal confidence level of simulated CI
#'  @param opacity scalar value for \code{alpha}
#'  @param fatten scalar passed to \code{geom_pointrange}
#'  @param title character vector for ggtitle
#'  @param targ_num desired number of lines on graph, for dynamic data thinning
#'  
#'  @return a ggplot object with simulated CI arranged about their target value
#'  
#'  @export 
rangePlot <- function(tf, target=mean(tf$est), orderFun=slug, conf=0.95
	, opacity=0.2, fatten=0.1, title="Range plot"
	, targ_num=1e3
){
  thinner<-max(floor(length(tf$est)/targ_num), 1)
  thinned<-tf[seq(thinner, length(tf$est), thinner),]
	return(ggplot(
		orderFun(thinned)
		, aes(x = quantile, y = est, ymin = lower, ymax = upper)
	)
		+ geom_pointrange(alpha = opacity, fatten = fatten
		                  , aes(color = ifelse(lower > target |upper < target, "red", "grey")))
	  + geom_hline(yintercept = target, color = "blue")
		+ geom_vline(
			xintercept = c((1 - conf)/2, 1 - (1 - conf)/2)
			, lty = 2, col = "red"
		)
		+ xlab("index")
		+ ylab("estimate")
		+ ggtitle(title)
	  + scale_x_continuous(expand = c(0,0)
	                       , breaks = c((1 - conf)/2, 0.25, 0.5, 0.75, conf + (1 - conf)/2)
	                       , labels = function(breaks){signif(breaks, 3)})
	  + scale_color_manual(values = c("grey", "red"))
	  + guides(color = F)
	  + theme_classic()
	)
}







