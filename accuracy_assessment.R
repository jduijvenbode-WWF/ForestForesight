library(terra)
library(sf)
library(sp)
library(raster)

setwd("E:/accuracy_assessment")
source("integratedalerts_filter.R")


## load RADD alerts and predictions from Deloitte's model
radd <- rast("gab415.tif")
prediciton <- vect("gabon contextualised 2023-04-15 no dups.shp")


## crop a small area (2 degree * 2 degree area of the upper left of "00N-010E" tile)
extent <- vect("aoi.shp")

radd_crop <- crop(radd,extent)
prediciton_crop <- crop(prediciton,extent)

## filter the RADD alerts within a certain period
radd_filtered <- alerts_filter("2023-04-15","2023-10-15",radd_crop)


## method 1: polygon based calculation
radd_mask <- mask(radd_filtered,prediciton_crop)
radd_vec <-vect(rasterToPolygons(raster(radd_mask),dissolve = FALSE))

count_TP <- 0
count_TP_fre <- 0


for (i in seq(1:dim(prediciton_crop)[1])) {
  result <- relate(radd_vec, prediciton_crop[i], "intersects")
  has_true_value <- any(result == TRUE)
  TP_frequency <- sum(result == TRUE)
  if (has_true_value == TRUE){
    count_TP <- count_TP + 1
    count_TP_fre <- count_TP_fre + TP_frequency
  }
}

print(count_TP)
print(count_TP_fre)

TP_FP <- dim(prediciton_crop)[1]
TP_FN <- sum(!is.na(as.matrix(radd_filtered)))

user_accuracy <- count_TP/TP_FP
producer_accuracy <- count_TP_fre/TP_FN

print(user_accuracy)
print(producer_accuracy)


## method 2: point based calculation
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
  TP_frequency2 <- sum(result2 == TRUE)
  if (has_true_value2 == TRUE){
    count_TP2 <- count_TP2 + 1
    count_TP_fre2 <- count_TP_fre2 + TP_frequency2
  }
}

print(count_TP2)
print(count_TP_fre2)

user_accuracy2 <- count_TP2/TP_FP
producer_accuracy2 <- count_TP_fre2/TP_FN

print(user_accuracy2)
print(producer_accuracy2)
















