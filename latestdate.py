import rasterio
from rasterio.windows import Window
import numpy as np
import sys
from scipy.ndimage import maximum_filter

def aggregate_by_40_max(input_array):

    small = input_array.reshape([int(input_array.shape[1]//40), 40,int(input_array.shape[1]//40), 40]).max(3).max(1)

    return small



def process_geotiff(input_file, output_suffix="_processed.tif"):
    # Open the GeoTIFF file
    with rasterio.open(input_file) as src:
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
            data[data > int(sys.argv[1])] = 0

            data=aggregate_by_40_max(data)
            

            # Create a new GeoTIFF file with the processed data
            res_col_offset=col_offset//40
            res_row_offset=row_offset//40
            print(res_row_offset,res_col_offset,result_array.shape)
            result_array[res_row_offset:(res_row_offset+(result_array.shape[0]//2)),res_col_offset:(res_col_offset+(result_array.shape[1]//2))]=data
        output_file = input_file.replace(".tif", f"_{output_suffix}")
        with rasterio.open(output_file, 'w', driver='GTiff', width=width, height=height, count=1, dtype=src.dtypes[0], crs=src.crs, transform=src.transform) as dst:
            dst.write(result_array.reshape(1,result_array.shape[0],result_array.shape[1]))


if __name__ == "__main__":
    print("Start")
    # Replace 'your_geotiff_file.tif' with the actual file path
    input_geotiff = 'C:/data/testpython.tif'
    process_geotiff(input_geotiff)