#' Prepare graticule object for ggplot of spatial objects
#' 
#' @description None
#' 
#' @param x Projected Spatial object 
#' @param ext extent Extent object for the plot (default is to use the extent of x)
#' 
#' @author Loic Dutrieux
#' 
#' @import rgeos
#' @import raster
#' @import sp
#' @import ggplot2
#' @import rgdal
#' 
# @export


ggraticule <- function(x, ext = extent(x)) {
    # Get extent as sp
    ext <- as(ext, 'SpatialPolygons')
    proj4string(ext) <- CRS(proj4string(x))
    extLL <- spTransform(ext, CRS('+proj=longlat +datum=WGS84'))
    
    # Now produce the actual lines (gridLines is a sp function, it has other arguments as well, see later)
    gridLL <- gridlines(extLL)
    grid <- spTransform(gridLL, CRS(proj4string(x)))
    # Tickmark locations
    extLine <- as(ext, 'SpatialLines')
    # TODO gINtersection for the two lines of the grid object separately will make things easier
    tick <- gIntersection(grid, extLine)
    # Assuming only bottom and left
    tickLeft <- tick[round(tick@coords[,1]) == round(extent(x)[1]),]
    tickBottom <- tick[round(tick@coords[,2]) == round(extent(x)[3]),]
    yLoc <- coordinates(tickLeft)[,2]
    xLoc <- coordinates(tickBottom)[,1]
    
    tickLeftLab <- coordinates(spTransform(tickLeft, CRS('+proj=longlat +datum=WGS84')))[,2]
    tickBottomLab <- coordinates(spTransform(tickBottom, CRS('+proj=longlat +datum=WGS84')))[,1]
    
    # convert grid to sldf (there is no fortify method for sl)
    griddf <- SpatialLinesDataFrame(grid, data = data.frame(rep(NA, length(grid))), match.ID = FALSE)
    ggrat <- fortify(griddf)
    
    list(ggrat = ggrat, x_at = xLoc, y_at = yLoc, xLab = tickBottomLab, yLab = tickLeftLab)
    
    
}
# 

x <- getData('GADM', country='Bol', level=1)

xSin <- spTransform(x, CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"))
ggxSin <- fortify(xSin)
ggrat <- ggraticule(xSin)

ggplot() +
    geom_polygon(data = ggxSin, aes(x=long, y=lat, group=group), fill = NA, colour = 'black') +
    geom_path(data = ggrat$ggrat, aes(x=long,y=lat,group=group), col = 'grey', linetype = 'dashed') +
    coord_equal(xlim = extent(xSin)[c(1,2)], ylim = extent(xSin)[c(3,4)]) +
    scale_x_continuous(name = 'Long', expand=c(0, 0), breaks = ggrat$x_at, labels = ggrat$xLab) +
    scale_y_continuous(name = 'Lat', expand=c(0, 0), breaks = ggrat$y_at, labels = ggrat$yLab) +
    theme_bw()


### Well, kind of ...
