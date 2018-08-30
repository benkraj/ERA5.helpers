#' Single level ERA5 extraction.
#'
#' This is a (mostly helper) function for extracting out single level (height) era5 data
#' @param file Filename
#' @param variable ERA5 variable name, in my case t2m (temp 2 meters) or tp (total precip)
#' @param lat Latitude of point (it will find closest ERA5 grid point)
#' @param long Longitude of point
#' @keywords ERA5
#' @export
#' @examples
#' era5extractsingle("t2m_201803.nc", "t2m", 12.5, 11.5)

era5extractsingle <- function(file, variable, lat, long) {
  
  require(ncdf4)
  require(ncdf4.helpers)
  require(ncdump)
  #Open Data File, and extract out list of longitudes/latitudes, and Date/Times.
  nc <- nc_open(file)
  latitude.full <- ncvar_get(nc, varid = "latitude")
  longitude.full <- ncvar_get(nc, varid = "longitude")
  
  #Choose lat/long closest to your chosen point
  lat_index <- which.min(abs(latitude.full - lat))
  lon_index <- which.min(abs(longitude.full - long))
  
  #Pull out data for your point
  var_extract <- nc.get.var.subset.by.axes(nc, variable,
                                           axis.indices = list(X=lon_index, Y=lat_index))
  nc_close(nc)
  
  #Put data into data frame
  df <- data.frame(variable=as.vector(var_extract)) 
  return(df)
  
  #Close connection to NC file
  nc_close(nc)
  closeAllConnections()
}

