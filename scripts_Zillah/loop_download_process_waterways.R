library(terra);library(sf);library(ohsome);library(ForestForesight)
data(gfw_tiles)
gfw_tiles=vect(gfw_tiles)
for (x in 1:nrow(gfw_tiles)) {
  tile = gfw_tiles[x,]
  print(tile$tile_id)
  print(Sys.time())
  
  tryCatch({
    a = ohsome_elements_geometry(
      boundary = st_bbox(c(xmin = xmin(tile), xmax = xmax(tile), ymax = ymax(tile), ymin = ymin(tile)), crs = st_crs(4326)), 
      filter = "(waterway= river or waterway=canal) and geometry:line", 
      time = "2021-01-01",
      clipGeometry = TRUE
    ) |> ohsome_post() |> svc()
    a = a[[1]]
    plot(a)
    start = Sys.time()
    
    tempras = rast(ncols = 10000, nrows = 10000, xmin = xmin(tile), xmax = xmax(tile), ymin = ymin(tile), ymax = ymax(tile))
    
    rasterize(simplifyGeom(a, tolerance = 10/110000), tempras, filename = "D:/ff-dev/results/temprasterizedwaterways.tif", datatype = "INT1U", NAflag = NA, overwrite = TRUE)
    
    system(paste("python", "C:/Users/admin/Documents/GitHub/ForestForesight/distance.py", "D:/ff-dev/results/temprasterizedwaterways.tif", "D:/ff-dev/results/temprasterizedwaterwaysdist.tif"), intern = TRUE)
    
    t3 = terra::aggregate(rast("D:/ff-dev/results/temprasterizedwaterwaysdist.tif"), 4, fun = "mean", 
                          filename = file.path("D:/ff-dev/results/waterwaydist/", paste0(tile$tile_id, "_waterwaydist_2021-01-01.tif")))
    
  }, error = function(e) {
    cat("An error occurred:", conditionMessage(e), "\n")
    # You can add additional handling or logging here
  })
}

print(Sys.time()-start)
