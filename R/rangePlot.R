#' Create Rangeplot Based On Order Function
#' 
#' Confidence intervals (CI) consisting of an estimate, upper, and lower limit
#' from Monte-Carlo sampling (given that the statistical hypothesis is true) are
#' arranged according to \code{orderFun} and compared with the hypothesized
#' (true) value of the statistic to examine statistical coverage and bias of CI
#' 
#' @template tf_template
#' @param target scalar, value of statistic under (true) hypothesis, or
#'   "sampleMean", default, which calculates the sample mean.
#' @param orderFun one of \code{c(slug, milli, blob)}, determining the way CI
#' are arranged in the plot
#' @param conf scalar in \[0,1\], nominal confidence level of simulated CI
#' @param opacity scalar value for \code{alpha}
#' @param fatten scalar passed to \code{geom_pointrange}
#' @param title character vector for ggtitle
#' @param targNum desired number of lines on graph, for dynamic data thinning
#'  
#' @importFrom stats median quantile runif pnorm pbinom rbinom rnorm 
#' @return a ggplot object with simulated CI arranged about their target value
#'  
#' @export 
rangePlot <- function(tf
                      , target = "sampleMean"
                      , orderFun = slug
                      , conf = 0.95
                      , opacity = 0.2
                      , fatten = 0.1
                      , title = "Range plot"
                      , targNum = 1e3){
  if(target == "sampleMean"){
    target <- mean(tf$est)
  }
  thinner <- max(floor(length(tf$est)/targNum), 1)
  thinned <- tf[seq(thinner
                    , length(tf$est)
                    , thinner)
                ,
                ]
  return(ggplot(
    orderFun(thinned)
    , aes(x = quantile, y = est, ymin = lower, ymax = upper)
    )
    + geom_pointrange(alpha = opacity, fatten = fatten
                      , aes(color = ifelse(lower > target | upper < target
                                           , "red", "grey")))
    + geom_hline(yintercept = target, color = "blue")
    + geom_vline(
      xintercept = c((1 - conf)/2, 1 - (1 - conf)/2)
      , lty = 2, col = "red"
    )
    + xlab("index")
    + ylab("estimate")
    + ggtitle(title)
    + scale_x_continuous(expand = c(0,0)
                         , breaks = c((1 - conf)/2, 0.25, 0.5, 0.75
                                      , conf + (1 - conf)/2)
                         , labels = function(breaks){signif(breaks, 3)})
    + scale_color_manual(values = c("grey", "red"))
    + guides(color = "none")
    + theme_classic()
  )
}
