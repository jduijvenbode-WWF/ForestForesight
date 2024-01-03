import argparse
import rasterio
import numpy as np
from scipy.ndimage import distance_transform_edt

def distance_to_nearest_nonzero_geotiff(input_geotiff, output_geotiff):
    # Read the GeoTIFF file
    with rasterio.open(input_geotiff) as src:
        # Read the data
        input_array = src.read(1)
        # Get the metadata for creating the output GeoTIFF
        metadata = src.profile

    # Create a binary mask where zeros are treated as foreground and non-zeros as background
    input_array[np.isnan(input_array)]=0
    mask = input_array == 0
    print(np.max(mask))
    print(np.max(input_array))

    # Calculate Euclidean distance transform
    distance_transform = distance_transform_edt(mask)

    # Update metadata for the output GeoTIFF
    metadata.update(dtype='float64', count=1)

    # Write the distance transform array to a new GeoTIFF file
    with rasterio.open(output_geotiff, 'w', **metadata) as dst:
        dst.write(distance_transform, 1)

def main():
    parser = argparse.ArgumentParser(description='Calculate Euclidean distance to nearest non-zero value in a GeoTIFF.')
    parser.add_argument('input_geotiff', help='Path to the input GeoTIFF with 1\'s and 0\'s.')
    parser.add_argument('output_geotiff', help='Path to the output GeoTIFF for storing the distance array.')

    args = parser.parse_args()

    distance_to_nearest_nonzero_geotiff(args.input_geotiff, args.output_geotiff)

if __name__ == "__main__":
    main()