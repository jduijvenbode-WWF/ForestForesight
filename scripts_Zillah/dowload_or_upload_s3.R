inputdir= "D:/ff-dev/results/preprocessed"
library(aws.s3)
tiles=list.files(file.path(inputdir), pattern ="_")

Sys.setenv("AWS_DEFAULT_REGION"="eu-west-1")

features= c("_2001-01-01_landpercentage.tif")

for (tile in tiles){
  for (feature in features){
    object_key <- paste0('preprocessed/',tile, '/', tile, feature)
    #del_key= paste0(tile, '/', feature)
    local_path <- file.path(inputdir, tile, paste0(tile,feature))
    print(paste("put", object_key,"to", local_path))
    #delete_object(object=del_key, bucket="wwf-ff-global")
    put_object(file=local_path, object=object_key, bucket="wwf-ff-global")
    #save_object(object = object_key, bucket = "wwf-ff-global", file = local_path, quiet = FALSE, overwrite=TRUE)
  }
}

for (tile in tiles){
  for (file in c("pop2025.tif")){
    local_path <- file.path(inputdir, tile, file)
    file.remove(local_path)
    print(paste("removed :", local_path))
  }

}

for (tile in tiles){
  for (feature in features){
    #local_path <- file.path(inputdir, tile, feature)
    #rast_feature= rast(local_path)
    object_key <- paste0(tile, '/', feature)
    rast_feature = s3read_using(FUN=rast,object=object_key, bucket = "wwf-ff-global" )
    if(!all(dim(rast_feature)== c(2500,2500,1))){
      print(object_key)
      print(dim(rast_feature))}
  }
}
