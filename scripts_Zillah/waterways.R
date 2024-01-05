library(terra)
library(sf)
library(ohsome)

# Set up parallel processing (optional)
terraOptions(plan = "multiprocess")

# Load forest mask raster
forest_mask <- rast("D:/ff-dev/results/20N_100E/forestmask2019.tif")>0

# Get the bounding box from the forest mask
bbox <- st_bbox(terra::ext(forest_mask))

# Retrieve data from ohsome API within the adjusted bounding box
a <- ohsome_elements_geometry(
  boundary = bbox,
  filter = "(waterway= river or waterway=canal) and geometry:line",
  time = "2023-12-01",
  clipGeometry = TRUE
) |> ohsome_post() |> svc()

# Extract the first layer
a <- a[[1]]
a= aggregate(a)
plot(a)
# Set up raster parameters using the resolution of the forest mask
tempras <-rast(ext(bbox), resolution= res(forest_mask))

forest_mask= crop(forest_mask,tempras)

# Simplify and rasterize waterways
t1 <- rasterize(simplifyGeom(a, tolerance = 10 / 110000), tempras)

# Calculate distance using the spatial index
start_time = Sys.time()
res1 <- distance(t1)
end_time = Sys.time()

print(start_time-end_time)

plot(res1)
