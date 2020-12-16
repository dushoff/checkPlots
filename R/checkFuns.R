#' Create a checkplot
#' 
#' Visual tool to determine if nominal p-values for Monte-Carlo simulations
#' under a given (null) hypothesis is true have uniform distribution
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
	return(ggplot(stats, aes(p))
		+ geom_histogram(breaks = breaks)
		+ geom_hline(yintercept = nrow(stats)/(facets * (length(breaks) - 1)))
		+ ggtitle(tag)
		+ labs(x = "nominal p-value")
	)
}