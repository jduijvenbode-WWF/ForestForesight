#######processors#####

labels=function(ras,diffdate,filename){
  if(!file.exists(filename)){
  m=c(20000+diffdate,20000+diffdate+183,1,
      30000+diffdate,30000+diffdate+183,1,
      40000+diffdate,40000+diffdate+183,1)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  aggregate(x=classify(ras,rcl=rclmat,others=NA),40,fun="max",na.rm=T,filename=filename,overwrite=T)
  }
}

mean_confidence=function(ras,diffdate,filename){
  if(!file.exists(filename)){
  m=c(20000+diffdate-365,19999+diffdate,1,
      30000+diffdate-365,29999+diffdate,2,
      40000+diffdate-365,39999+diffdate,4)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  aggregate(x=classify(ras,rcl=rclmat,others=NA),40,fun="mean",na.rm=T,filename=filename,overwrite=T)
  }
}

lastsixmonths=function(ras,diffdate,filename){
  if(!file.exists(filename)){
  m=c(20000+diffdate-180,19999+diffdate,1,
      30000+diffdate-180,29999+diffdate,1,
      40000+diffdate-180,39999+diffdate,1)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  aggregate(classify(ras,rcl=rclmat,others=NA),40,fun="sum",na.rm=T,filename=filename,overwrite=T)
  }
}
lastthreemonths=function(ras,diffdate,filename){
  if(!file.exists(filename)){
    m=c(20000+diffdate-90,19999+diffdate,1,
        30000+diffdate-90,29999+diffdate,1,
        40000+diffdate-90,39999+diffdate,1)
    rclmat <- matrix(m, ncol=3, byrow=TRUE)
    aggregate(classify(ras,rcl=rclmat,others=NA),40,fun="sum",na.rm=T,filename=filename,overwrite=T)
  }
}
twelvetosixmonths=function(ras,diffdate,filename){
  if(!file.exists(filename)){
  m=c(20000+diffdate-365,19999+diffdate-182,1,
      30000+diffdate-365,29999+diffdate-182,1,
      40000+diffdate-365,39999+diffdate-182,1)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  aggregate(classify(ras,rcl=rclmat,others=NA),40,fun="sum",na.rm=T,filename=filename,overwrite=T)
  }
}
forestmask=function(file,ras,filename){
  if(!file.exists(filename)){
  mask=rast(file,win=ext(ras))
  tempfile=gsub(".tif","temp.tif",filename)
  newmask=project(mask,ras,filename=tempfile,overwrite=T)
  aggregate(newmask,40,fun="sum",na.rm=T,filename=filename,overwrite=T)
  }
}
totaldeforestation=function(ras,diffdate,filename){
  if(!file.exists(filename)){
  m=c(20000,19999+diffdate,1,
      30000,29999+diffdate,1,
      40000,39999+diffdate,1)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  tempfile=gsub(".tif","temp.tif",filename)
  ras=classify(ras,rcl=rclmat,others=0,filename=tempfile,overwrite=T)
  aggregate(ras,40,fun="sum",na.rm=T,filename=filename,overwrite=T)
  file.remove(tempfile)
  }
}
latestdeforestation=function(ras,diffdate,filename){
  if(!file.exists(filename)){
aggregate(diffdate-(ras%%10000),40,fun="max",na.rm=T,filename=filename,overwrite=T)
  }
}

smoothed_deforestation=function(inputfile,window_matrix,filename){
  if(!file.exists(filename)){
  smoothed=focal(rast(inputfile)>0,w=window_matrix/sum(window_matrix),na.policy="omit",
                 na.rm=T,filename=filename,overwrite=T)
  }
}

patchsize=function(ras,window_matrix,filename="patchsize.tif"){
  if(!file.exists(filename)){
  aggregate(focal(ras>0,w=window_matrix,na.policy="omit",
                  na.rm=T),40,fun="sum",filename=filename,overwrite=T)
  }
}

smoothed_6months=function(inputfile="6months.tif",window_matrix,filename="deforestation_smoothed6months.tif"){
  if(!file.exists(filename)){
  focal(rast(inputfile)>0,w=window_matrix/sum(window_matrix),
        na.policy="omit",na.rm=T,filename=filename,overwrite=T)
  }
}
dateras=function(ras,diffdate,filename="dateras.tif"){
  ras[]=sin(diffdate/(365/(2*pi)))
  writeRaster(ras,filename)
}
edgedetection_withmask=function(deforestationfile,maskfile=NA,diffdate,filename="edgedensity.tif"){
  if(!file.exists(filename)){
  pastdef=rast(maskfile)-rast(deforestationfile)
  # sobel_x <- matrix(c(-1, 0, 1, -2, 0, 2, -1, 0, 1), nrow = 3, byrow = TRUE)
  # sobel_y <- matrix(c(-1, -2, -1, 0, 0, 0, 1, 2, 1), nrow = 3, byrow = TRUE)
  # cat(Sys.time())
  # cat("\n")
  # # Apply the Sobel filter using the focal function for both X and Y directions
  # sobel_x_result <- focal(pastdef, w = matrix(sobel_x, nrow = 3), focalFun = sum)
  # sobel_y_result <- focal(pastdef, w = matrix(sobel_y, nrow = 3), focalFun = sum)
  # cat(Sys.time())
  # cat("\n")
  # Calculate the gradient magnitude
  gradient_magnitude <- boundaries(pastdef,filename="boundaries_rough.tif",overwrite=T)
  aggregate(gradient_magnitude,40,fun="sum",na.rm=T,filename=filename,overwrite=T)
  }
}



############helpers###########
createfilename=function(rasname,date=NA,layer,number=NA){
  date=as.character(date)
  if(!is.na(number)){if(!is.na(date)){date=paste0(date,"_",number)}else{date=number}}
  filename=paste0(rasname,"/",layer,if(!is.na(date)){paste0("_",date)}else{""},".tif")
  if(!dir.exists(rasname)){dir.create(rasname)}
  cat(paste0(filename,"\n"))
  return(filename)
}
createrasname=function(ras){
  rasname=gsub(".tif","",basename(ras))
  return(rasname)
}

splitintoparts=function(r,n){
  extlist=list()
  ext=ext(rast(r))
  for(i in ext[1]+((ext[2]-ext[1])/sqrt(n))*(seq(sqrt(n))-1)){
    for(j in ext[3]+((ext[4]-ext[3])/sqrt(n))*(seq(sqrt(n))-1)){
      extlist=append(extlist,ext(c(i,i+((ext[2]-ext[1])/sqrt(n)),j,j+((ext[4]-ext[3])/sqrt(n)))))
    }
  }
  return(extlist)
}
# Create an empty matrix filled with 0s
matrixcreator=function(size){
  matrix_size <- size
  center <- (matrix_size + 1) / 2
  window_matrix <- matrix(0, nrow = matrix_size, ncol = matrix_size)
  
  # Define the radius for the circular window
  radius <- (matrix_size - 1) / 2
  # Fill in the circular window
  for (i in 1:matrix_size) {
    for (j in 1:matrix_size) {
      distance_to_center <- sqrt((i - center)^2 + (j - center)^2)
      if (distance_to_center <= radius) {
        window_matrix[i, j] <- 1/max(distance_to_center,1)
      }
    }
  }
  return(window_matrix)
}



#######evaluation functions########
evalerrorF05 <- function(preds, dts_matrix) {
  # Check for NAs in preds and labels
  if (any(is.na(preds)) || any(is.na(getinfo(dts_matrix, "label")))) {
    stop("NA values detected in preds or labels.")
  }
  i <- 0.5
  labels <- getinfo(dts_matrix, "label")
  #cat(max(preds))
  a <- table((preds > i) * 2 + (labels > 0))
  if (length(a)==4){
    UA<<-a[4]/(a[3]+a[4])
    PA<<-a[4]/(a[2]+a[4])
    F05<<- 1.25 * UA * PA / (0.25 * UA + PA)  
  }else{
    F05 = 0
  }
  return(list(metric = "error F05", value = as.numeric(F05)))
}