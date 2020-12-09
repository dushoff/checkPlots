## mb.R - compiled by RoxygenReady, a package by @vertesy


#' compute p-values, CI for binomial proportions
#' 
#' A wrapper for several frequentist tests for binomial proportions, it provides 
#' consistent outputs regardless of test method.
#' 
#'
#' @param dat integer vector, observed number of successes
#' @param prob0 scalar; true probability of success
#' @param n  scalar, number of trials
#' @param testv character, which binomial test to use for estimating CI and p
#' 
#' @return dataframe same length as `dat` with p-value estimates, upper and lower 
#' confidence limits, and the name of the test used
#' 
#' 
#' @examples 
#' 
#' #set parameters
#' p_true <- 0.7
#' n_trials <- 10
#' n_samples <- 10
#' 
#' #generate data
#' mydat <- rbinom(n_samples, n_trials, p_true)
#' 
#' run tests
#' multBinom(mydat, ptrue, n_trials, testv = "binom.test")
#' multBinom(mydat, ptrue, n_trials, testv = "wald")
#' 
#' @export 

multBinom <- function(dat, prob0, n, testv = c("binom.test", "accept", "chisq", "wald")) {
	if (testv == "binom.test") {
		df <- map_dfr(dat, function(d) {
			bt <- binom.test(d, n, p = prob0, alternative = "less")
			gt <- binom.test(d, n, p = prob0, alternative = "greater")
			cp <- bt$p.value
			rp <- cp + runif(1) * (1 - gt$p.value - cp)
			ci <- binom.test(d, n, p = prob0, alternative = "two.sided")
			return(data.frame(est = d/n
			                  , cp
			                  , p = rp
			                  , upper = ci$conf.int[2]
			                  , lower = ci$conf.int[1]
			                  , test = testv))
		})
	}
	if (testv == "accept") {
		df <- future_map_dfr(dat, function(d) {
			p <- acceptbin(d, n, p = prob0)
			ci <- acceptinterval(d, n)
			return(data.frame(est = d/n
			                  , p
			                  , upper = ci[2]
			                  , lower = ci[1]
			                  , test = testv))
		})
	}
	if (testv == "wald") {
		df <- future_map_dfr(dat, function(d) {
			p <- pnorm((d/n - prob0)/(((d/n) * ((n - d)/n))^0.5 * n^(-0.5)))
			ci <- prop.test(d, n, p = prob0, alternative = "two.sided")
			return(data.frame(est = d/n
			                  , p
			                  , upper = ci$conf.int[2]
			                  , lower = ci$conf.int[1]
			                  , test = testv))
		})
	}
	return(df)
}

