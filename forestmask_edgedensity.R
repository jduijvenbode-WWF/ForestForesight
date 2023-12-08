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
#this should prioritize the tiles on the countries that we want to do first
files=list.files("D:/ff-dev/forestloss_2000_2022/",full.names=T)
areas=vect("D:/ff-dev/integratedalerts.geojson")
borders=vect("D:/ff-dev/results/borders.geojson")
prio_tifs=paste0(areas[borders[which(
  borders$iso3 %in% c("GAB","PER","COL","BOL","LAO","IND"))]]$tile_id,".tif")
files = c(files[which(basename(files) %in% basename(prio_tifs))], files[-which(basename(files) %in% basename(prio_tifs))])
#reclassification matrix
m=c(1,19,0)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
for(file in files){
  loc=gsub(".tif","",(basename(file)))
  print(loc)
  edgesfile=paste0("D:/ff-dev/results/",loc,"/edgedensity2019.tif")
    elefile=list.files(paste0("D:/ff-dev/results/",loc),full.names = T)[1]
    if(file.exists(elefile)){
      loss=rast(file)
      mask=rast(gsub("forestloss_2000_2022","forestmasks_tiled",file))
      #the original mask has values 1-100 for the fraction. Set everything that is deforested between 2001-2019
      # to 0 and multiply the rest by 100 so that you get a value between 0-10000 for fraction
        mask=mask*classify(loss,rclmat,others=100)
        template=rast(elefile)
        forestmask=paste0("D:/ff-dev/results/",loc,"/forestmask2019.tif")
        # reproject with the average, so that pixels have a value between 0-10000. write as unsigned 16bit
        #if(!file.exists(forestmask)){}
        project(mask,template,method="average",filename=forestmask,datatype="INT2U", overwrite=TRUE)
        print(paste("forest mask", loc ,"is created"))
        #mask=sobel_filter(mask)
        #project(mask,template,method="sum",filename=edgesfile)
    }
  }

