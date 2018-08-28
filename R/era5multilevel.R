#' Multi level ERA5 extraction.
#'
#' This is a (mostly helper) function for extracting out multi level (height) era5 data
#' @param file Filename
#' @param variable ERA5 variable name, in my case RH, wind direction, and wind speed
#' @param lat Latitude of point (it will find closest ERA5 grid point)
#' @param long Longitude of point
#' @keywords ERA5
#' @export
#' @examples
#' era5multilevel("rh_201803.nc", "rh", 12.5, 11.5)

era5multilevel <- function(file, variable, lat, long) {
  ##This is a function for extracting out multilevel era5 data
  require(ncdf4)
  require(ncdf4.helpers)
  require(ncdump)
  require(plyr)
  require(tidyverse)
  #Open NetCDF (.nc) format Data File, and extract out list of latitude/longitude within it.
  nc <- nc_open(file)
  latitude.full <- ncvar_get(nc, varid = "lat")
  longitude.full <- ncvar_get(nc, varid = "lon")
  
  
  #Choose the latitude/longitude closest to your location of interest from the list
  lat_index <- which.min(abs(latitude.full - lat))
  lon_index <- which.min(abs(longitude.full - long))
  
  #Pull out data of the variable of interest for your point.
  var_extract <- nc.get.var.subset.by.axes(nc, variable,
                                           axis.indices = list(X=lon_index, Y=lat_index))
  
  nc_close(nc)
  #Reshape numeric output into data frame
  df <- matrix(var_extract, 
               ncol = 12,
               byrow = TRUE)
  
  df <- as.data.frame(df)
  return(df)
  
  #Close connection to NC file
  nc_close(nc)
  closeAllConnections()
} 


