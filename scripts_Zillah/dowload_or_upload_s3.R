
library(aws.s3)
tiles=list.files(file.path(inputdir), pattern ="_")

Sys.setenv("AWS_DEFAULT_REGION"="eu-west-1")

features= c("pop2020.tif","pop2025.tif","pop2030.tif")

for (tile in tiles){
  for (feature in features){
    #object_key <- paste0(tile, '/', feature)
    #del_key= paste0(tile, '/lastloss.tif')
    local_path <- file.path(inputdir, tile, feature)
    to_path <-paste0("D:/ff-dev/population_tiles/",tile,'/', feature)
    #print(paste("Put", local_path, "to", object_key))
    #delete_object(object=del_key, bucket="wwf-ff-global")
    #put_object(file=local_path, object=object_key, bucket="wwf-ff-global")
    #save_object(object = object_key, bucket = "wwf-ff-global", file = local_path, quiet = FALSE, overwrite=TRUE)
    dir.create(paste0("D:/ff-dev/population_tiles/",tile))
    file.rename(from=local_path, to=to_path )
  }
}

for (tile in tiles){
  local_path <- file.path(inputdir, tile, "lastloss.tif")
  file.remove(local_path)
}


