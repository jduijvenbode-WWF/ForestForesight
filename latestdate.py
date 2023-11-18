import rasterio
from rasterio.windows import Window
import numpy as np
import argparse



def aggregate_by_40_max(input_array):

    small = input_array.reshape([int(input_array.shape[1]//40), 40,int(input_array.shape[1]//40), 40]).max(3).max(1)

    return small



def process_geotiff(input_file, output_file,relative_date):
    # Open the GeoTIFF file
    with rasterio.open(input_file) as src:
        newtransform=src.transform*src.transform.scale(40,40)
        # Get the dimensions of the raster
        width = src.width
        height = src.height

        # Calculate the number of windows (4 equal parts)
        num_windows = 4
        window_width = width // 2
        window_height = height // 2

        # Iterate over windows
        for i in range(num_windows):
            # Calculate the starting coordinates of the window
            col_offset = (i % 2) * window_width
            row_offset = (i // 2) * window_height

            # Define the window
            window = Window(col_offset, row_offset, window_width, window_height)
            # Read the data within the window
            data = src.read(window=window)
            if i==0:
                result_array=np.zeros((data.shape[1]//20,data.shape[2]//20))

            # Take the remainder of 10000 for every pixel
            data = np.remainder(np.nan_to_num(data), 10000)

            # Replace values lower than 0 with 0
            np.subtract(relative_date, data, out=data, where=data>0)
            np.maximum(data,0,out=data)

            data=aggregate_by_40_max(data)
            

            # Create a new GeoTIFF file with the processed data
            res_col_offset=col_offset//40
            res_row_offset=row_offset//40
            print(res_row_offset,res_col_offset,result_array.shape)
            result_array[res_row_offset:(res_row_offset+(result_array.shape[0]//2)),res_col_offset:(res_col_offset+(result_array.shape[1]//2))]=data
        with rasterio.open(output_file, 'w', driver='GTiff',compress='LZW', width=width//40, height=height//40, count=1, dtype=src.dtypes[0], crs=src.crs, transform=newtransform) as dst:
            dst.write(result_array.reshape(1,result_array.shape[0],result_array.shape[1]))


if __name__ == "__main__":
    # Create a command-line argument parser
    parser = argparse.ArgumentParser(description="Apply Sobel filter to a geotiff image.")
    parser.add_argument("input_image", help="Path to the input geotiff image")
    parser.add_argument("output_image", help="Path to the output geotiff image")
    parser.add_argument("relative_date", help="relative date")
    args = parser.parse_args()
    print("Start")
    # Replace 'your_geotiff_file.tif' with the actual file path
    input_geotiff =  args.input_image
    output_geotiff = args.output_image
    reldate=int(args.relative_date)
    process_geotiff(input_geotiff,output_geotiff,reldate)