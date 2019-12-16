
## In the middle of modularizing: checkplot should be wpPlot and should call pianoplot
checkplot <- function(stats, breaks=seq(0,1,0.05), tag="", Wmin=0, facets=1){
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
rangePlot <- function(tf, target=mean(tf$est), orderFun=slug, alpha=0.05
	, opacity=0.2, fatten=0.1, title="Range plot"
){
	return(ggplot(
		orderFun(tf)
		, aes(x=quantile, y=est, ymin = lower, ymax=upper)
	)
		+ geom_pointrange(alpha=opacity, fatten=fatten
		                  , aes(color=ifelse(lower>target |upper<target, "red", "grey")))
		+ geom_hline(yintercept=0)
	  + geom_hline(yintercept=target, color="blue")
		+ geom_vline(
			xintercept=c(alpha/2, 1-alpha/2)
			, lty=2, col="red"
		)
		+ xlab("index")
		+ ylab("estimate")
		+ ggtitle(title)
	  + scale_color_manual(values=c("grey", "red"))
	  + guides(color=F)
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
