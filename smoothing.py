import sys
import numpy as np
import rasterio
from rasterio.transform import from_origin
from scipy.ndimage import convolve

def weighted_smoothing(data, window_size):
    # Create a weighted distance matrix
    x, y = np.meshgrid(np.arange(window_size), np.arange(window_size))
    distance = np.sqrt((x - window_size // 2)**2 + (y - window_size // 2)**2)
    weight = 1.0 / (1.0 + distance)  # Weighted distance

    # Normalize the weights
    weight /= weight.sum()

    # Apply convolution with the weighted kernel
    smoothed_data = convolve(np.nan_to_num(data), weight, mode='constant', cval=0.0)

    return smoothed_data

def main():
    # Check if the correct number of command-line arguments is provided
    if len(sys.argv) != 3:
        print("Usage: python script.py input_geotiff output_geotiff")
        sys.exit(1)

    # Read data from the input GeoTIFF file
    try:
        input_file = sys.argv[1]
        output_file = sys.argv[2]

        with rasterio.open(input_file) as src:
            data = src.read(1)  # Assuming a single-band raster

            # Get geospatial information from the source raster
            profile = src.profile
            transform = profile["transform"]
            crs = profile["crs"]

        # Apply weighted smoothing with a window size of 21 squared
        smoothed_data = weighted_smoothing(data, window_size=21)

        # Update the profile for the output GeoTIFF
        profile.update(dtype=rasterio.float32, count=1)

        # Write the smoothed data to the output GeoTIFF
        with rasterio.open(output_file, 'w', **profile) as dst:
            dst.write(smoothed_data, 1)

        print("Smoothing operation completed successfully.")

    except Exception as e:
        print(f"An error occurred: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()