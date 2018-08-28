#' Temperature, RH, and Wind extraction 
#'
#' This function allows you to extract out just the Temperature, RH, and Wind from ERA5 data---can be used with map to do many points on a grid (but beware of too many files open)
#' @param latitude Latitude of point (it will find closest ERA5 grid point)
#' @param longitude Longitude of point
#' @param date.start Start of ERA5 data---NOT date of interest---date in file
#' @param date.end As above
#' @keywords ERA5
#' @export
#' @examples
#' dry.2015 <- map2_dfr(grid.subset2$Var1, grid.subset2$Var2, era5.temp.rh.wind.extract, date.start = "2015-03-01", date.end="2015-05-31")
#' This is pulling out all temps, RH, and winds (converting to u/v wind) for each point in the grid from every night of interest.

era5.temp.rh.wind.extract <- function(latitude, longitude, date.start, date.end){
  require(ncdf4)
  require(ncdf4.helpers)
  require(ncdump)
  require(plyr)
  require(tidyverse)
  require(lubridate)
  
  #single level extraction
  t2m <- ldply(list.files(pattern="t2m_"), 
               era5extractsingle, 
               variable="t2m", 
               lat=latitude, long=longitude)
  colnames(t2m) <- c("t2m")
  
  tp <- ldply(list.files(pattern="tp_"), 
              era5extractsingle, 
              variable="tp", 
              lat=latitude, long=longitude)
  colnames(tp) <- c("tp")
  
  
  #multi-level extraction
  rh <- ldply(list.files(pattern="rh_"), 
              era5multilevel, 
              variable="RH", 
              lat=latitude, long=longitude)
  
  #the multiple level files have their columns named with a height. We use these estimates of height levels
  colnames(rh) <- c("rh400", "rh350", "rh300", "rh255", "rh215", 
                    "rh180", "rh145", "rh115", "rh85", "rh55", "rh32", "rh10")
  #Wind Direction
  wd <- ldply(list.files(pattern="wd_"), 
              era5multilevel, 
              variable="WD", 
              lat=latitude, long=longitude)
  colnames(wd) <- c("wd400", "wd350", "wd300", "wd255", "wd215", "wd180", "wd145", "wd115", "wd85", "wd55",
                    "wd32", "wd10")
  
  #Wind Speed
  ws<- ldply(list.files(pattern="ws_"), 
             era5multilevel, 
             variable="WS", 
             lat=latitude, long=longitude)
  colnames(ws) <- c("ws400", "ws350", "ws300", "ws255", "ws215", "ws180", "ws145", "ws115", "ws85", "ws55",
                    "ws32", "ws10")
  
  #Adding date column
  seq.years <- seq(from = as.POSIXct(paste0(date.start,"00:00:00"), tz='Africa/Bamako'), 
                   to = as.POSIXct(paste0(date.end,"23:00:00"), tz='Africa/Bamako'),by="hours")
  
  #everything is bound into a location-wise dataframe. It should be large, 35064 rows, 40 columns
  df <- cbind(seq.years, t2m, rh, tp, wd, ws)
  
  #fixing some column name issues
  
  df <- df %>%
    mutate(time = strftime(seq.years, format="%H:%M", tz="Africa/Bamako"),
           round.date = as.Date(seq.years),
           grp.month=month(seq.years),
           plot.night = case_when(
             time %in% c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00") ~ round.date-1, TRUE ~ round.date)) %>%
    filter(time %in% c("19:00", "20:00", "21:00", "22:00", "23:00", "00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00")) %>%
    select(seq.years, time, plot.night, t2m, rh180, tp, wd180, ws180) %>%
    mutate(u.wind=-ws180 * sin(2 * pi * wd180/360), #from https://www.researchgate.net/profile/Stuart_Grange2/publication/262766424_Technical_note_Averaging_wind_speeds_and_directions/links/54f6184f0cf27d8ed71d5bd4/Technical-note-Averaging-wind-speeds-and-directions.pdf
           v.wind=-ws180 * cos(2 * pi * wd180/360)) %>%
    group_by(plot.night) %>%
    summarise(wd.average = (atan2(mean(u.wind, na.rm=TRUE), mean(v.wind, na.rm=TRUE)) * 360/2/pi) + 180,
              scalar.mean.ws.180 = mean(ws180),
              mean.t2m = mean(t2m)-273, #switch from Kelvin
              mean.rh180 = mean(rh180),
              cum.tp = sum(tp)) %>%
    mutate(latitude=latitude,
           longitude=longitude) %>%
    select(plot.night, latitude, longitude, mean.t2m, mean.rh180, cum.tp, wd.average, scalar.mean.ws.180)
  return(df)
  closeAllConnections()
}