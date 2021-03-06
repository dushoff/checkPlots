#########################################
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
# ####### I like the checkplot function better:
# # it takes a dataframe as its arg that can be the same df as rangePlot
# pianoPlot <- function(pvec, breaks=seq(0,1,0.05), tag=""){
#   stats <- data.frame(p=pvec)
#   return(ggplot(stats, aes(p))
#          + geom_histogram(breaks=breaks)
#          + geom_hline(yintercept=nrow(stats)/(length(breaks)-1))
#          + ggtitle(tag)
#   )
# }

#########################
# some hidden ggplot stuff to make rangePlot facetable
#make the proto for a new "stat"
StatSlug<- ggproto("StatSlug", Stat
                   , compute_group = function(data, scales, orderFun=slug, targ_num=1e3, target=NA) {
                       myt = ifelse(!is.na(target), ifelse(target %in% names(data), mean(data[,target, drop=T]), mean(data$est)), mean(data$est))
                       thinner = max(floor(length(data$est)/targ_num), 1)
                       thinned = data[seq(thinner, length(data$est), thinner),, drop=T]
                       statdf = orderFun(thinned, thinned$est, thinned$upper, thinned$lower)
                       # print(!is.na(target))
                       # print(target)
                       # print(colnames(data))
                       # print(target %in% colnames(data))
                       # View(data)
                       
                       print(myt)
                       statdf %>% mutate(nogood=ifelse(upper<myt|myt<lower, "red", "grey"), myt=myt)
                     #if we want these aes to be required (which allows specifying your own names) then want the functions to be like this
                   }
                   , required_aes = c("est", "target")
                   # , default_aes =
)

#explain hwo to make it in ggplot
stat_slug <- function(mapping = NULL, data = NULL, geom = "pointrange"
                      , position = "identity", na.rm = FALSE, show.legend = NA 
                      , inherit.aes = T, ...) {
  ggplot2::layer(
    stat = StatSlug, data = data
    , mapping = aes(y=est, ymin=lower, ymax=upper, x=stat(quantile), color=stat(nogood))
    , geom = geom
    , position = position
    , show.legend = show.legend
    , inherit.aes = inherit.aes
    , params = list(na.rm = na.rm, ...)
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
rangePlot <- function(tf, orderFun=slug, conf=0.95
                      , opacity=0.2, fatten=0.1, title="Range plot"
                      , targ_num=1e3, target=NA
)
  {   (ggplot(tf, aes(est=est, upper=upper, lower=lower))
    + stat_slug(target=target
                , targ_num=targ_num, alpha=opacity, fatten=fatten)
                    # + geom_hline(yintercept=0)
  + geom_smooth(aes(y=est, x=runif(length(est)))
                , method="lm", formula=y~1, se=F, color="blue", size=0.5)
  # + geom_hline(yintercept=target, color="green")
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



milli <- function(tf, est="est", upper=upper, lower=lower){ #milli just uses rank estimate
  numEst <- nrow(tf)
  return(tf
         %>% arrange(est)
         %>% mutate(
           quantile=((1:numEst)-1/2)/numEst
         )
  )
}

blob <- function(tf, est="est", upper=upper, lower=lower){ #blob uses the upper bound when above median, or lower bound when below
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

slug <- function(tf, est=est, upper=upper, lower=lower){ #slug orders by the bound you care about, dynamically
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
