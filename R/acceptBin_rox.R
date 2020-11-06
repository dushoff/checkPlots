## acceptBin.R - compiled by RoxygenReady, a package by @vertesy


#' acceptbin 
#' 
#' 
#' @param x 
#' @param n 
#' @param p 
#' @examples acceptbin (x =  , n =  , p =  )
#' @export 

acceptbin <- function(x, n, p){
	p1 <- 1 - pbinom(x - 1, n, p)
	p2 <- pbinom(x, n, p)
	a1 <- p1 + pbinom(qbinom(p1, n, p) - 1, n, p)
	a2 <- p2 + 1 - pbinom(qbinom(1 - p2, n, p), n, p)
	return(min(a1, a2))
}


#' acceptinterval 
#' 
#' 
#' @param x 
#' @param n 
#' @param level 
#' @param tolerance 
#' @examples acceptinterval (x =  , n =  , level = 0.95, tolerance = 1e-04)
#' @export 

acceptinterval <- function (x, n, level = 0.95, tolerance = 1e-04) {
	lower <- 0
	upper <- 1
	if (x != 0) {
		lower <- qbeta((1 - level)/2, x, n - x + 1)
		while (acceptbin(x, n, lower) <= (1 - level)) {
			lower <- lower + tolerance
		}
	}
	if (x != n) {
		upper - qbeta(1 - (1 - level)/2, x + 1, n - x)
		while (acceptbin(x, n, upper) <= (1 - level)) {
			upper <- upper - tolerance
		}
	}
	c(lower, upper)
}


