#' RGB spatial plot with ggplot
#' 
#' @description Produces an object to feed into ggplot geom_raster or geom_tile to produce a rgb plot
#' 
#' @param x RasterStackBrick object 
#' @param r Numeric Band number to be plotted in red
#' @param g Numeric Band number to be plotted in green 
#' @param b Numeric Band number to be plotted in blue
#' 
#' @author Loic Dutrieux (using some code snippets by Josh Gray and Robert Hijmans)
#' 
#' @import magrittr
#' @import raster
#' @import dplyr
#' 
#' @export
#'
#' @examples
#' 
#' library(ggplot2)
#' library(raster)
#' 
#' x <- brick(system.file("external/rlogo.grd", package="raster"))
#' 
#' ggrgb <- ggRGB(x, 3,2,1)
#' 
#' ggplot() + 
#'     geom_raster(data = ggrgb, aes(x=x, y=y, fill=values)) +
#'     scale_fill_identity() +
#'     coord_equal() +
#'     scale_x_continuous(name = 'Lat', expand=c(0, 0)) +
#'     scale_y_continuous(name = 'Long', expand=c(0, 0))
#'     

ggRGB <- function(x, r, g, b) {
    
    # Linear strech function
    .linStretchVec <- function(x) {
        v <- quantile(x, c(0.02, 0.98), na.rm = TRUE)
        temp <- (255 * (x - v[1]))/(v[2] - v[1])
        temp[temp < 0] <- 0
        temp[temp > 255] <- 255
        return(temp)
    }
    
    names(x)[r] <- 'r'
    names(x)[g] <- 'g'
    names(x)[b] <- 'b'
    out <- x%>%
        fortify() %>%
        mutate(r = .linStretchVec(values.r)) %>%
        mutate(g = .linStretchVec(values.g)) %>%
        mutate(b = .linStretchVec(values.b)) %>%
        transmute(x, y, values = rgb(r, g, b, maxColorValue=255))
    
    return(out)
        
}







