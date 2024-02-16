library(terra);library(sf);library(ohsome);library(ForestForesight)

data(gfw_tiles)
gfw_tiles=vect(gfw_tiles)
for (x in 1:nrow(gfw_tiles)) {
  for(date in c("2024-01-01")){
    tile = gfw_tiles[x,]
    print(tile$tile_id)
    print(date)
    print(Sys.time())
    newfilename=file.path("C:/data/roaddist/", paste0(tile$tile_id, "_roaddist_",date,".tif"))
    if(!file.exists(newfilename)){
      tryCatch({
        a = ohsome_elements_geometry(
          boundary = st_bbox(c(xmin = xmin(tile), xmax = xmax(tile)-5, ymax = ymax(tile), ymin = ymin(tile)), crs = st_crs(4326)),
          filter = "type:way and highway=* and geometry:line",
          time = date,
          clipGeometry = TRUE
        ) |> ohsome_post() |> svc()
        b = ohsome_elements_geometry(
          boundary = st_bbox(c(xmin = xmin(tile)+5, xmax = xmax(tile), ymax = ymax(tile), ymin = ymin(tile)), crs = st_crs(4326)),
          filter = "type:way and highway=* and geometry:line",
          time = date,
          clipGeometry = TRUE
        ) |> ohsome_post() |> svc()
      }, error = function(e) {
        cat("An error occurred:", conditionMessage(e), "\n")
        # You can add additional handling or logging here
      })
      if(exists("a")){
        a = rbind(a[[1]],b[[1]])

        start = Sys.time()

        tempras = rast(ncols = 10000, nrows = 10000, xmin = xmin(tile), xmax = xmax(tile), ymin = ymin(tile), ymax = ymax(tile))
        if(file.exists("C:/data/temprasterizedroads.tif")){file.remove("C:/data/temprasterizedroads.tif")}
        rasterize(simplifyGeom(a, tolerance = 10/110000), tempras, filename = "C:/data/temprasterizedroads.tif", datatype = "INT1U", NAflag = NA, overwrite = TRUE)
        system(paste("python", "C:/data/git/ForestForesight/preprocessing/distance.py", "C:/data/temprasterizedroads.tif", "C:/data/temprasterizedroadsdist.tif"), intern = TRUE)

        t3 = terra::aggregate(rast("C:/data/temprasterizedroadsdist.tif"), 4, fun = "mean",
                              filename = newfilename)


        rm(a)
      }
    }
  }
}


# tempras=sapply(list.dirs(path = "C:/data/colombia_tiles/input/",full.names = T)[2:104],function(x) list.files(x,full.names=T)[1])
# roadfiles=list.files("C:/data/roaddist/",full.names = T)
# for(rfile in roadfiles){
#   tras=tempras[grep(substr(basename(rfile),1,8),tempras)]
#   if(!(identical(ext(rast(tras))[1:4],ext(rast(rfile))[1:4]))){
#     file.remove(rfile)
#   }
# }
