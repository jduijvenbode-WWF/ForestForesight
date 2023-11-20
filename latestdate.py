import rasterio
from rasterio.windows import Window
import numpy as np
import argparse
import os



def aggregate_by_40_max(input_array,fun):
    if fun=="max":
        small = input_array.reshape([int(input_array.shape[1]//40), 40,int(input_array.shape[1]//40), 40]).max(3).max(1)
    elif fun=="mean":
        small = input_array.reshape([int(input_array.shape[1]//40), 40,int(input_array.shape[1]//40), 40]).mean(3).mean(1)
    elif fun=="nanmean":
        small = np.nan_to_num(np.nanmean(np.nanmean(input_array.reshape([int(input_array.shape[1]//40), 40,int(input_array.shape[1]//40), 40]),axis=3),axis=1))
    elif fun=="sum":
        small = input_array.reshape([int(input_array.shape[1]//40), 40,int(input_array.shape[1]//40), 40]).sum(3).sum(1)
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
        latest_deforestation_file=output_file.replace("layer","latestdeforestation")
        create_latest_deforestation= not os.path.isfile(latest_deforestation_file)
        threemonths_file=output_file.replace("layer","3months")
        create_threemonths = not os.path.isfile(threemonths_file)
        sixmonths_file=output_file.replace("layer","6months")
        create_sixmonths = not os.path.isfile(sixmonths_file) 
        twelvetosixmonths_file=output_file.replace("layer","12-6months")
        create_twelvetosixmonths = not os.path.isfile(twelvetosixmonths_file)
        totaldeforestation_file=output_file.replace("layer","totaldeforestation")
        create_totaldeforestation = not os.path.isfile(totaldeforestation_file)
        groundtruth_file=output_file.replace("layer","groundtruth")
        create_groundtruth = not os.path.isfile(groundtruth_file)
        confidence_file=output_file.replace("layer","confidence")
        create_confidence = not os.path.isfile(confidence_file)
        # Iterate over windows
        if any([create_confidence,create_groundtruth,create_totaldeforestation,create_sixmonths,create_threemonths,create_twelvetosixmonths,create_latest_deforestation]):
            for i in range(num_windows):
                # Calculate the starting coordinates of the window
                col_offset = (i % 2) * window_width
                row_offset = (i // 2) * window_height
                offy1=col_offset//40
                offx1=row_offset//40

                # Define the window
                window = Window(col_offset, row_offset, window_width, window_height)
                # Read the data within the window
                data = src.read(window=window)
                if i==0:
                    template=np.zeros((data.shape[1]//20,data.shape[2]//20))
                    latest_deforestation=template.copy()
                    threemonths=template.copy()
                    sixmonths=template.copy()
                    twelvetosixmonths=template.copy()
                    totaldeforestation=template.copy()
                    groundtruth=template.copy()
                    confidence=template.copy()
                offx2=(offx1+(template.shape[0]//2))
                offy2=(offy1+(template.shape[1]//2))


                # Take the remainder of 10000 for every pixel
                if create_confidence: confidence[offx1:offx2,offy1:offy2]=aggregate_by_40_max((np.remainder(np.nan_to_num(data), 10000)<relative_date).astype(int)*data//10000,fun="nanmean")
                data = np.remainder(np.nan_to_num(data), 10000)


                #remove current date from data to get relative date, ignoring 0's, then remove everything below 0 to remove future deforestation. then aggregate by 40.     
                if create_latest_deforestation: latest_deforestation[offx1:offx2,offy1:offy2]=aggregate_by_40_max(np.multiply(np.minimum(np.divide(data,relative_date, where=data>0),1),10000).astype(int),fun="max")

                #remove current date from data to get relative date, ignoring 0's, then remove everything below 0 to remove future deforestation. then aggregate by 40.  
                if create_threemonths: threemonths[offx1:offx2,offy1:offy2]=aggregate_by_40_max(((data<=relative_date)&(data>(relative_date-92))).astype(int),fun="sum")
                if create_sixmonths: sixmonths[offx1:offx2,offy1:offy2]=aggregate_by_40_max(((data<=relative_date)&(data>(relative_date-183))).astype(int),fun="sum")
                if create_twelvetosixmonths: twelvetosixmonths[offx1:offx2,offy1:offy2]=aggregate_by_40_max(((data<=(relative_date-183))&(data>(relative_date-366))).astype(int),fun="sum")
                if create_totaldeforestation: totaldeforestation[offx1:offx2,offy1:offy2]=aggregate_by_40_max((data<=(relative_date)).astype(int),fun="sum")
                if create_groundtruth: groundtruth[offx1:offx2,offy1:offy2]=aggregate_by_40_max(((data<=(relative_date+182))&(data>relative_date)).astype(int),fun="max")

                   
            if create_latest_deforestation:
                with rasterio.open(latest_deforestation_file, 'w', driver='GTiff',compress='LZW', width=width//40, height=height//40, count=1, dtype=src.dtypes[0], crs=src.crs, transform=newtransform) as dst:
                    dst.write(latest_deforestation.reshape(1,latest_deforestation.shape[0],latest_deforestation.shape[1]))


            if create_threemonths:
                with rasterio.open(threemonths_file, 'w', driver='GTiff',compress='LZW', width=width//40, height=height//40, count=1, dtype=src.dtypes[0], crs=src.crs, transform=newtransform) as dst:
                    dst.write(threemonths.reshape(1,threemonths.shape[0],threemonths.shape[1]))

            
            if create_sixmonths:
                with rasterio.open(threemonths_file, 'w', driver='GTiff',compress='LZW', width=width//40, height=height//40, count=1, dtype=src.dtypes[0], crs=src.crs, transform=newtransform) as dst:
                    dst.write(sixmonths.reshape(1,sixmonths.shape[0],sixmonths.shape[1]))

            
            if create_twelvetosixmonths:
                with rasterio.open(twelvetosixmonths_file, 'w', driver='GTiff',compress='LZW', width=width//40, height=height//40, count=1, dtype=src.dtypes[0], crs=src.crs, transform=newtransform) as dst:
                    dst.write(twelvetosixmonths.reshape(1,twelvetosixmonths.shape[0],twelvetosixmonths.shape[1]))

            
            if create_totaldeforestation:
                with rasterio.open(totaldeforestation_file, 'w', driver='GTiff',compress='LZW', width=width//40, height=height//40, count=1, dtype=src.dtypes[0], crs=src.crs, transform=newtransform) as dst:
                    dst.write(totaldeforestation.reshape(1,totaldeforestation.shape[0],totaldeforestation.shape[1]))

            
            if create_groundtruth:
                with rasterio.open(groundtruth_file, 'w', driver='GTiff',compress='LZW', width=width//40, height=height//40, count=1, dtype=src.dtypes[0], crs=src.crs, transform=newtransform) as dst:
                    dst.write(groundtruth.reshape(1,groundtruth.shape[0],groundtruth.shape[1]))

            
            if create_confidence:
                with rasterio.open(confidence_file, 'w', driver='GTiff',compress='LZW', width=width//40, height=height//40, count=1, dtype="float32", crs=src.crs, transform=newtransform) as dst:
                    dst.write(confidence.reshape(1,confidence.shape[0],confidence.shape[1]))

if __name__ == "__main__":
    # Create a command-line argument parser
    parser = argparse.ArgumentParser(description="Apply calculation to a geotiff image.")
    parser.add_argument("input_image", help="Path to the input geotiff image")
    parser.add_argument("output_image", help="Path to the output geotiff image")
    parser.add_argument("relative_date", help="relative date")
    args = parser.parse_args()
    # Replace 'your_geotiff_file.tif' with the actual file path
    input_geotiff =  args.input_image
    output_geotiff = args.output_image
    reldate=int(args.relative_date)
    process_geotiff(input_geotiff,output_geotiff,reldate)