library(terra)
sobel_filter <- function(raster_data) {
    # Apply Sobel filter to the raster
  sobel_x <- matrix(c(-1, 0, 1, -2, 0, 2, -1, 0, 1), nrow = 3, ncol = 3)
  sobel_y <- t(sobel_x)
  result_x <- terra::focal(raster_data, sobel_x, fun="mean", expand=T)
  result_y <- terra::focal(raster_data, sobel_y, fun="mean", expand=T)
  
  # Combine the x and y gradient matrices
  gradient_magnitude <- sqrt(result_x^2 + result_y^2)
  
  # Convert the result back to raster
  
  return(gradient_magnitude)
}
files=list.files("D:/ff-dev/forestloss_2000_2022/",full.names=T)

m=c(1,19,1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
for(file in files){
  loc=gsub(".tif","",(basename(file)))
  print(loc)
  edgesfile=paste0("D:/ff-dev/results/",loc,"/edgedensity2019.tif")
  if(!file.exists(edgesfile)){
    elefile=list.files(paste0("D:/ff-dev/results/",loc),full.names = T)[1]
    if(file.exists(elefile)){
      loss=rast(file)
      mask=rast(gsub("forestloss_2000_2022","forestmasks_tiled",file))
        mask=mask-classify(loss,rclmat,others=0)
        template=rast(elefile)
        forestmask=paste0("D:/ff-dev/results/",loc,"/forestmask2019.tif")
        if(!file.exists(forestmask)){project(mask,template,method="sum",filename=forestmask)}
        mask=sobel_filter(mask)
        project(mask,template,method="sum",filename=edgesfile)
      }
    }
  }

