library(terra)
# List all folders and subfolders in the input data
# be sure to be in the R/ workspace or change the relative path below
tif_files <- list.files("..//test_data//preprocessed/input", pattern = "\\.tif$", full.names = TRUE, recursive = TRUE)

# Loop through each file and compress it in-place
for (tif_file in tif_files) {
  cat("compressing file: ", tif_file, " \n")
  # Load the .tif file
  r <- rast(tif_file)

  # Create a temporary file to write the compressed .tif
  temp_file <- paste0(tempfile(), ".tif")

  # Write the compressed file to the temporary location
  writeRaster(r, temp_file, gdal = c("COMPRESS=DEFLATE"), overwrite = TRUE)

  # Replace the original file with the compressed version
  file.rename(temp_file, tif_file)
}
