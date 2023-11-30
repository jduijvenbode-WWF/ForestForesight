### set your own work environment first
setwd("....")

### load needed library and functions
library(terra)
library(sf)
library(sp)
library(raster)
source("integratedalerts_filter.R")

## load RADD alerts and predictions from Deloitte's model
radd <- rast("...")          # RADD alerts (raster)
prediciton <- vect("...")      # the predictions from Deloitte's model (shapefile)


## crop a small area 
extent <- vect("...")  # define your interested area 

radd_crop <- crop(radd,extent)
prediciton_crop <- crop(prediciton,extent)


## filter the RADD alerts within a certain period
radd_filtered <- alerts_filter("xxxx-xx-xx","xxxx-xx-xx",radd_crop)  


# method 1: polygon based calculation
#           In this method, we compare the filtered alerts with each polygon in the predictions.
#           If there is an intersection relationship existing, then we count once and we compute
#           the frequency of the intersection as well.
#           Limitation: the method will overestimate the user accuracy and producer accuracy.This
#                       is because the alerts can be in different polygons at the same time. Therefore,
#                       one alert can be counted as TP(true positive) multiple times. 


radd_mask <- mask(radd_filtered,prediciton_crop)
radd_vec <-vect(rasterToPolygons(raster(radd_mask),dissolve = FALSE))   

count_TP <- 0
count_TP_fre <- 0


for (i in seq(1:dim(prediciton_crop)[1])) {
  result <- relate(radd_vec, prediciton_crop[i], "intersects")
  # make a judgement on whether there is an overlap between the alerts and one prediction 
  has_true_value <- any(result == TRUE)      
  # if it is true, count once and calculate the frequency of the overlap in each polygon
  if (has_true_value == TRUE){
    TP_frequency <- sum(result == TRUE)
    count_TP <- count_TP + 1
    count_TP_fre <- count_TP_fre + TP_frequency
  }
}


TP_FP <- dim(prediciton_crop)[1]       ## total number of polygons in the prediction
TP_FN <- sum(!is.na(as.matrix(radd_filtered)))  ## total number of alerts in a certain period

user_accuracy <- count_TP/TP_FP
producer_accuracy <- count_TP_fre/TP_FN

print(user_accuracy)
print(producer_accuracy)


# method 2: point based calculation
#           In this method, we take the centre point of alerts as our input data to compare with 
#           each polygon in the prediction. If there is an intersection relationship existing, then
#           we count once and we compute the frequency of the intersection as well.
#           Limitation: the method will underestimate the user accuracy and producer accuracy.This
#                       is because the centre point (alerts) can be out of the predictions. Then,
#                       those alerts won't be counted in both user accuracy and producer accuracy 
#                       calculations.

radd_point <- rasterToPoints(raster(radd_mask))

points_df <- data.frame(
  x = radd_point[, 1],
  y = radd_point[, 2]
)

points_sf <- st_as_sf(points_df, coords = c("x", "y"))
st_crs(points_sf) <- st_crs(crs(radd_mask))
radd_vec_point <- vect(points_sf)


count_TP2 <- 0
count_TP_fre2 <- 0


for (i in seq(1:dim(prediciton_crop)[1])) {
  result2 <- relate(radd_vec_point, prediciton_crop[i], "intersects")
  has_true_value2 <- any(result2 == TRUE)
  
  if (has_true_value2 == TRUE){
    TP_frequency2 <- sum(result2 == TRUE)
    count_TP2 <- count_TP2 + 1
    count_TP_fre2 <- count_TP_fre2 + TP_frequency2
  }
}


user_accuracy2 <- count_TP2/TP_FP     
producer_accuracy2 <- count_TP_fre2/TP_FN

print(user_accuracy2)
print(producer_accuracy2)
















