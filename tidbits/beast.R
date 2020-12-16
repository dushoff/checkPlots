
library(ggplot2); theme_set(theme_bw())
library(dplyr)

beast <- function(reps, samps, blength=21){
	for (r in 1:reps){
		print(pianoPlot(
			runif(samps)
			, tag = paste(samps, "Samples; Rep =", r)
			, breaks = seq(0, 1, length.out=blength)
		))
	}
}

set.seed(1104)

beast(10, 1000)
beast(10, 2000)
beast(10, 5000)
beast(10, 10000)
