
library(aws.s3)
tiles=list.files(file.path(inputdir), pattern ="_")

Sys.setenv("AWS_DEFAULT_REGION"="eu-west-1")

features= c("pop2020.tif","pop202.tif","pop2030.tif")

for (tile in tiles){
  for (feature in features){
    object_key <- paste0(tile, '/', feature)
    del_key= paste0(tile, '/', feature)
    local_path <- file.path(inputdir, tile, feature)
    print(paste("delete:", del_key))
    delete_object(object=del_key, bucket="wwf-ff-global")
    #put_object(file=local_path, object=object_key, bucket="wwf-ff-global")
    #save_object(object = object_key, bucket = "wwf-ff-global", file = local_path, quiet = FALSE, overwrite=TRUE)
  }
}

for (tile in tiles){
  for (file in c("pop2020.tif","pop202.tif","pop2030.tif")){
    local_path <- file.path(inputdir, tile, file)
    file.remove(local_path)
    print(paste("removed :", local_path))
  }
  
}


