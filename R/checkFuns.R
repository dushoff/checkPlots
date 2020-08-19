
## In the middle of modularizing: checkplot should be wpPlot and should call pianoplot

#' Create a checkplot
#' 
#' Visual tool to determine if nominal p-values for Monte-Carlo simulations under a given (null) hypothesis is true have uniform distribution 
#' 
#' @importFrom ggplot2 aes, geom_histogram, geom_line, ggtitle, labs, geom_pointrange, scale_x_continuous, geom_hline, geom_vline, xlab, ylab, scale_color_manual, theme_classic, guides, ggplot
#' 
#' @importfrom dplyr mutate, arrange
#' 
#' @param stats Dataframe with a column called `p` containing nominal p-values
#' @param breaks Numeric vector in \[0,1\], determining bar widths for geom_histogram
#' 
#' @param tag optional character string for a ggtitle
#' @param Wmin scalar, to filter the df "stats"
#' @param facets Integer, number of facets if using facet_wrap or similar
#' 
#' @return ggplot object, a checkplot
#' @aliases checkplot
#' 
#' @export
#' @examples 
#' 

checkPlot <- function(stats, breaks = seq(0,1,0.05), tag = "", Wmin = 0, facets = 1){
	# stats <- filter(stats, W>Wmin)
	return(ggplot(stats, aes(p))
		+ geom_histogram(breaks=breaks)
		+ geom_hline(yintercept=nrow(stats)/(facets*(length(breaks)-1)))
		+ ggtitle(tag)
		+ labs(x="nominal p-value")
	)
}
####### I like the checkplot function better:
# it takes a dataframe as its arg that can be the same df as rangePlot
pianoPlot <- function(pvec, breaks=seq(0,1,0.05), tag=""){
	stats <- data.frame(p=pvec)
	return(ggplot(stats, aes(p))
		+ geom_histogram(breaks=breaks)
		+ geom_hline(yintercept=nrow(stats)/(length(breaks)-1))
		+ ggtitle(tag)
	)
}

## rangePlots are named for their order functions:
## slug, blob and milli (for millipede)
## slug is currently preferred

#I think this needs to be a bit smarter. 
#First, we need to thin the the number of reps some to get a clear picture. 
#The number of reps needed for a flat checkplot is much more.
#I think about 1e3 is the right number to make a slugplot.Obviously not exact
#Second, we want to mark alpha/2 and 1-(alpha/2) more clearly
#rangeplots don't facet well. Giving up on that. 
rangePlot <- function(tf, target=mean(tf$est), orderFun=slug, conf=0.95
	, opacity=0.2, fatten=0.1, title="Range plot"
	, targ_num=1e3
){
  thinner<-max(floor(length(tf$est)/targ_num), 1)
  thinned<-tf[seq(thinner, length(tf$est), thinner),]
	return(ggplot(
		orderFun(thinned)
		, aes(x=quantile, y=est, ymin = lower, ymax=upper)
	)
		+ geom_pointrange(alpha=opacity, fatten=fatten
		                  , aes(color=ifelse(lower>target |upper<target, "red", "grey")))
		# + geom_hline(yintercept=0)
	  + geom_hline(yintercept=target, color="blue")
		+ geom_vline(
			xintercept=c((1-conf)/2, 1-(1-conf)/2)
			, lty=2, col="red"
		)
		+ xlab("index")
		+ ylab("estimate")
		+ ggtitle(title)
	  + scale_x_continuous(expand=c(0,0), breaks=c((1-conf)/2, 0.25, 0.5, 0.75, conf+(1-conf)/2)
	                       , labels=function(breaks){signif(breaks, 3)})
	  + scale_color_manual(values=c("grey", "red"))
	  + guides(color=F)
	  + theme_classic()
	)
}

milli <- function(tf){ #milli just uses rank estimate
	numEst <- nrow(tf)
	return(tf
		%>% arrange(est)
		%>% mutate(
			quantile=((1:numEst)-1/2)/numEst
		)
	)
}

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
