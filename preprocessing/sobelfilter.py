import rasterio
import numpy as np
from scipy import ndimage
import argparse

# Create a command-line argument parser
parser = argparse.ArgumentParser(description="Apply Sobel filter to a geotiff image.")
parser.add_argument("input_image", help="Path to the input geotiff image")
parser.add_argument("output_image", help="Path to the input geotiff image")
args = parser.parse_args()

# Load the input geotiff image
input_image_path = args.input_image

with rasterio.open(input_image_path, 'r') as src:
    # Read the image as a NumPy array
    input_image = src.read(1)

    # Apply the Sobel filter for edge detection
    sobel_x = ndimage.sobel(input_image, axis=0, mode='reflect')
    sobel_y = ndimage.sobel(input_image, axis=1, mode='reflect')

    # Combine the horizontal and vertical gradients to get the magnitude
    magnitude = np.sqrt(sobel_x**2 + sobel_y**2)
    magnitude = magnitude[1:-1, 1:-1]
    # Create an output geotiff with the same metadata
    profile = src.profile
    output_image_path = args.output_image
    with rasterio.open(output_image_path, 'w', **profile) as dst:
        dst.write(magnitude, 1)