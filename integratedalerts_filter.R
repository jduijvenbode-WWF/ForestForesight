# This function is used to filter the integrated alerts within a particular time interval.
# The input data should be: the start date and the end date of the time interval, besides that an integrated alerts raster is needed.
# The format of the input date is string(str), for example: "2015-01-01"
# The output of this function is a filtered raster with integrated alerts within the given time interval.


library(terra)

alerts_filter <- function(startdate,enddate,integrated_alerts){
  start_encode <- substr(gsub(x="2015-01-01",pattern="-", replacement=""), start=5, stop=8)
  startdate_renew <- as.numeric(as.Date(startdate) - as.Date("2015-01-01"))
  time_interval <- as.numeric(as.Date(enddate) - as.Date(startdate))
  
  startdate_low <- as.numeric(paste0("2",start_encode)) + startdate_renew
  startdate_high <- as.numeric(paste0("3",start_encode)) + startdate_renew
  startdate_highest <- as.numeric(paste0("4",start_encode)) + startdate_renew
  
  enddate_low <- startdate_low + time_interval
  enddate_high <- startdate_high + time_interval
  enddate_highest <- startdate_highest + time_interval
  
  m=c(startdate_low,enddate_low,1,
      startdate_high,enddate_high,2,
      startdate_highest,enddate_highest,3)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  inalerts_sel = terra::classify(integrated_alerts,rcl=rclmat,others=NA)
  
  return(inalerts_sel)
}




