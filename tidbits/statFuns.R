library(ggplot2); theme_set(theme_bw())

reps <- 1e4
samps <- 30
rfun <- rcauchy

## To get good CIs, use the standard test
## This requires P value fiddling for checkplots
oneT <- function(x, m=0){
		t <- t.test(x-m)
		p <- (1-sign(t$statistic)*(1-t$p.value))/2
		w <- shapiro.test(x)$statistic
		return(c(p=p, t$est, ci=t$conf.int, W=w))
}

twoT <- function(x, y){
		t <- t.test(x, y)
		p <- (1-sign(t$statistic)*(1-t$p.value))/2
		return(c(p=p, t$est, ci=t$conf.int))
}

nvec <- numeric(reps)
onevec <- numeric(reps)
twovec <- numeric(reps)
for (i in 1:reps){
	x <- rfun(samps)
	y <- rfun(samps)
	## z <- rnorm(samps)
	## nvec[[i]] <- oneT(z)[["p.t"]]
	## onevec[[i]] <- oneT(x, m=1)[["p.t"]]
	twovec[[i]] <- twoT(x, y)[["p.t"]]
}

## print(pianoPlot(nvec))
## print(pianoPlot(onevec))
print(pianoPlot(twovec))

