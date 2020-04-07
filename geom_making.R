#########################################
# next big project here is to add a geom )or at least a stat) somehow. Going to learn as I go. 

#Now I think the first step is making a stat. 
# https://stackoverflow.com/questions/55811839/extending-ggplot2-with-a-custom-geometry-for-sf-objects/55823898#55823898

StatSlug<- ggproto("StatSlug", Stat
                   , compute_group = function(data, scales, orderFun="slug", targ_num=1e3 ) {
                       thinner<-max(floor(length(data$est)/targ_num), 1)
                       thinned<-data[seq(thinner, length(data$est), thinner),]
                       orderFun(thinned$est, thinned$upper, thinned$lower)
                     #if we want these aes to be required (which allows specifying your own names) then want the functions to be like this
                          }
                   , required_aes = c("est", "upper", "lower")
)



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

milli <- function(tf, est=est){ #milli just uses rank estimate
  numEst <- nrow(tf)
  return(tf
         %>% arrange(est)
         %>% mutate(
           quantile=((1:numEst)-1/2)/numEst
         )
  )
}

blob <- function(tf, est=est, upper=upper, lower=lower){ #blob uses the upper bound when above median, or lower bound when below
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

slug <- function(tf, upper=upper, lower=lower){ #slug orders by the bound you care about, dynamically
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

#ggproto
function (`_class` = NULL, `_inherit` = NULL, ...) 
{
  e <- new.env(parent = emptyenv())
  members <- list(...)
  if (length(members) != sum(nzchar(names(members)))) {
    abort("All members of a ggproto object must be named.")
  }
  if (length(members) > 0) {
    list2env(members, envir = e)
  }
  `_inherit` <- substitute(`_inherit`)
  env <- parent.frame()
  find_super <- function() {
    eval(`_inherit`, env, NULL)
  }
  super <- find_super()
  if (!is.null(super)) {
    if (!is.ggproto(super)) {
      abort("`_inherit` must be a ggproto object.")
    }
    e$super <- find_super
    class(e) <- c(`_class`, class(super))
  }
  else {
    class(e) <- c(`_class`, "ggproto", "gg")
  }
  e
}

stat_smooth<-function (mapping = NULL, data = NULL, geom = "smooth", position = "identity", 
          ..., method = NULL, formula = NULL, se = TRUE, n = 80, span = 0.75, 
          fullrange = FALSE, level = 0.95, method.args = list(), na.rm = FALSE, 
          orientation = NA, show.legend = NA, inherit.aes = TRUE) 
{
  layer(data = data, mapping = mapping, stat = StatSmooth, 
        geom = geom, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = list(method = method, 
                                                 formula = formula, se = se, n = n, fullrange = fullrange, 
                                                 level = level, na.rm = na.rm, orientation = orientation, 
                                                 method.args = method.args, span = span, ...))
}

?StatSmooth
