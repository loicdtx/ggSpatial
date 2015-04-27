#' Fortify methods for raster* objects
#' 
#' @param x raster object
#' 
#' @author Loic Dutrieux
#' 
#' @import raster
#' @import magrittr
#' 
#' @name fortify.Raster
#' 
#' @examples 
#' 
#' library(raster)
#' library(ggplot2)
#'
#' r <- raster(nrow = 10, ncol = 20)
#' r[] <- rnorm(ncell(r))
#' 
#' ggr <- fortify(r)
#' 
#' ggplot() +
#'     geom_raster(data = ggr, aes(x,y,fill = values)) +
#'     coord_equal() +
#'     scale_x_continuous(name = 'Lat', expand=c(0, 0)) +
#'     scale_y_continuous(name = 'Long', expand=c(0, 0))
#' 
#' # Also works for rasterStackBricks
#' 
#' b <- brick(nrow = 10, ncol = 20, nl = 3)
#' b[] <- rnorm(ncell(b) * nlayers(b))
#' names(b) <- c('a', 'b', 'c')
#' ggb <- fortify(b)
#' 
#' ggplot() +
#'     geom_raster(data = ggb, aes(x,y,fill = values.a)) +
#'     coord_equal() +
#'     scale_x_continuous(name = 'Lat', expand=c(0, 0)) +
#'     scale_y_continuous(name = 'Long', expand=c(0, 0))
#' 
#' # For facet grid/wrap, you can use tidyr to reshape the dataframe produced by the function
#' library(tidyr)
#' 
#' ggb2 <- gather(ggb, layer, values, -x, -y)
#' 
#' ggplot() +
#'     geom_raster(data = ggb2, aes(x,y,fill = values)) +
#'     coord_equal() +
#'     scale_x_continuous(name = 'Lat', expand=c(0, 0)) +
#'     scale_y_continuous(name = 'Long', expand=c(0, 0))+
#'     facet_wrap(~ layer)
#'     
#' @export
#' @rdname fortify.Raster

fortify.Raster <- function(x) {
    
    xy <- xyFromCell(x, seq_len(ncell(x)))
    out <- x %>%
        getValues() %>%
        data.frame(values = .) %>%
        cbind(xy)
    return(out)
}


