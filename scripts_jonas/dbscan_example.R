# Install and load required packages
if (!requireNamespace("terra", quietly = TRUE)) {
  install.packages("terra")
}

if (!requireNamespace("fpc", quietly = TRUE)) {
  install.packages("fpc")
}

library(terra)
library(fpc)

# Create a simple example SpatVector
coords <- cbind(runif(100), runif(100))
pts <- vect(coords)
crs(pts) <- "+proj=longlat +datum=WGS84"

# Plot the original points
plot(pts, col = "blue", main = "Original Points")

# Extract coordinates
coordinates <- crds(pts)

# Use DBSCAN to cluster points
dbscan_result <- dbscan(coordinates, eps = 0.1, MinPts = 5)

# Assign cluster labels to the original SpatVector
pts$cluster <- dbscan_result$cluster

# Plot the clustered points
plot(pts, col = pts$cluster, main = "DBSCAN Clustering")

# Display cluster information
cat("Number of clusters:", length(unique(dbscan_result$cluster)), "\n")