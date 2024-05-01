library(lubridate)
library(terra)
maxval=global(rast("D:/ff-dev/alerts/10S_070W.tif")%%1e4,"max",na.rm=T)
start_date <- ymd("2015-01-01")

# Add 50 days
result_date <- start_date + days(maxval)
cat(paste("latest date of IA alerts (According to 10S_070W):",result_date,"\n feel free to process till",floor_date(result_date, unit = "month"),"\n"))
