#' Subset ERA5 grid to polygon
#'
#' This is a function to find the extents of a polygon, find the closest grid points (i.e. from ERA5) inside these extents and output the data
#' @param ncfile ncfile to get grid --- use a multilevel file or change the lat/long names
#' @param polygon.df Dataframe of polygon with columns lat and long
#' @keywords ERA5
#' @export
#' @examples
#' koulikoro.grid <- subset.grid.to.polygon(ncfile="2018/wd_201803.nc", polygon.df = out_df )

grid.to.polygon <- function(ncfile, polygon.df){
  require(ncdf4)
  require(ncdf4.helpers)
  require(ncdump)
  require(tidyverse)
  #inputs are an ncfile (i.e. wd_), and a polygon.df also with columns lat and long
  
  #first pull out grid
  nc <- nc_open(ncfile)
  
  latitude.full <- ncvar_get(nc, varid='lat')
  longitude.full <- ncvar_get(nc, varid = "lon")
  
  grid <- expand.grid(lat=latitude.full, long=longitude.full)
  
  nc_close(nc)
  closeAllConnections()
  
  #then find min/max range of grid
  max.lat <- max(polygon.df$lat)
  min.lat <- min(polygon.df$lat)
  
  max.long <- max(polygon.df$long)
  min.long <- min(polygon.df$long)
  
  #find closest points to the min/max
  index.max.lat <- grid$lat[which.min(abs(grid$lat-max.lat))]
  index.min.lat <- grid$lat[which.min(abs(grid$lat-min.lat))]
  
  index.max.long <- grid$long[which.min(abs(grid$long-max.long))]
  index.min.long <- grid$long[which.min(abs(grid$long-min.long))]
  
  
  grid.lat.from.polygon <- grid$lat[grid$lat <= index.max.lat & grid$lat >= index.min.lat]
  grid.long.from.polygon <- grid$long[grid$long <= index.max.long & grid$long >= index.min.long]
  
  expanded.grid <- expand.grid(lat=grid.lat.from.polygon, long=grid.long.from.polygon) %>%
    distinct()
  return(expanded.grid)
}