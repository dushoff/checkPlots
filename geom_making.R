#########################################
# next big project here is to add a geom somehow. Going to learn as I go. 

#code from the web https://github.com/tidyverse/ggplot2/wiki/Creating-a-new-geom
GeomField <- proto(Geom, {
    draw <- function(., data, scales, coordinates, ...) 
    {
        # draw the field grobs in relation to the data
    }
    
    draw_legend <- function(., data, ...) {
        # show a grob in the key that represents the data
    }
    
    objname <- "field" # name of the geom in lowercase. Must correspond to GeomField.
    desc <- "Single line segments"
    
    default_stat <- function(.) StatIdentity
    required_aes <- c("x", "y", "angle", "length") 
    default_aes <- function(.) aes(colour="black", angle=pi/4, length=1, size=0.5, linetype=1)
    guide_geom <- function(.) "field"
    
    icon <- function(.) # a grob representing the geom for the webpage
        
        desc_params <- list( # description of the (optional) parameters of draw
        )
    
    seealso <- list(
        geom_path = GeomPath$desc,
        geom_segment = GeomPath$desc,
        geom_line = GeomLine$desc
    )
    
    examples <- function(.) {
        # examples of the geom in use
    }
    
})

draw <- function(., data, scales, coordinates, arrow=NULL, ...) {
    with(coordinates$transform(data, scales), 
         fieldGrob(x, y, angle, length, size, 
                   col=colour, linetype, arrow)
    )    
}

geom_slug<-
    

