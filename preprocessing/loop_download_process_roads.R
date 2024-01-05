library(terra);library(sf);library(ohsome);library(ForestForesight)
data(gfw_tiles)
gfw_tiles=vect(gfw_tiles)
for (x in 1:nrow(gfw_tiles)) {
  for(date in c("2022-01-01","2023-01-01")){
  tile = gfw_tiles[x,]
  print(tile$tile_id)
  print(date)
  print(Sys.time())
  
  tryCatch({
    a = ohsome_elements_geometry(
      boundary = st_bbox(c(xmin = xmin(tile), xmax = xmax(tile), ymax = ymax(tile), ymin = ymin(tile)), crs = st_crs(4326)), 
      filter = "type:way and highway=* and geometry:line", 
      time = date,
      clipGeometry = TRUE
    ) |> ohsome_post() |> svc()
    a = a[[1]]
    
    start = Sys.time()
    
    tempras = rast(ncols = 10000, nrows = 10000, xmin = xmin(tile), xmax = xmax(tile), ymin = ymin(tile), ymax = ymax(tile))
    
    rasterize(simplifyGeom(a, tolerance = 10/110000), tempras, filename = "C:/data/temprasterizedroads.tif", datatype = "INT1U", NAflag = NA, overwrite = TRUE)
    
    system(paste("python", "C:/data/git/ForestForesight/distance.py", "C:/data/temprasterizedroads.tif", "C:/data/temprasterizedroadsdist.tif"), intern = TRUE)
    
    t3 = terra::aggregate(rast("C:/data/temprasterizedroadsdist.tif"), 4, fun = "mean", 
                          filename = file.path("C:/data/roaddist/", paste0(tile$tile_id, "_roaddist_",date,".tif")))
    
  }, error = function(e) {
    cat("An error occurred:", conditionMessage(e), "\n")
    # You can add additional handling or logging here
  })
  }
}

print(Sys.time()-start)
