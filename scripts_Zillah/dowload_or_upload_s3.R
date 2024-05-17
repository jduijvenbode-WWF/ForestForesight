inputdir= "D:/ff-dev/results/preprocessed"
library(aws.s3)
tiles=list.files(file.path(inputdir), pattern ="_")
Sys.setenv("AWS_DEFAULT_REGION"="eu-west-1")

feature= "_lastmonth.tif"
dates = daterange("2021-01-01","2023-12-01")
for (tile in tiles){
  for (date in dates){
    object_key <- paste0('preprocessed/',tile, '/', paste0(tile,"_",date, feature))
    # del_key= paste0(tile, '/', feature)
    local_path <- file.path(inputdir, tile, paste0(tile,"_",date, feature))
    #delete_object(object=object_key, bucket="wwf-ff-global")
    #print(paste("put", local_path,"to", object_key))
    #put_object(file=local_path, object=object_key, bucket="wwf-ff-global") NEEEEE
    save_object(object = object_key, bucket = "wwf-ff-global", file = local_path, quiet = FALSE, overwrite=TRUE)
    print(paste("save", object_key ,"in", local_path))
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
  files = list.files(file.path(paste0(inputdir,"/", tile)), pattern="lastmonth")
  for (file in files){
    #local_path <- file.path(inputdir, tile, feature)
    #rast_feature= rast(local_path)
    object_key <- paste0(inputdir,'/', tile, '/', file)
    rast_feature = rast(object_key)
    rast_feature[rast_feature==65535]=0
    writeRaster(rast_feature, filename = object_key, overwrite = TRUE)
    print(object_key)
  }
}
