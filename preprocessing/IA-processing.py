import rasterio
from rasterio.windows import Window
import numpy as np
import argparse
import os
from scipy.ndimage import label
import time
from scipy.ndimage import convolve

def weighted_smoothing(data, window_size):
    # Create a weighted distance matrix
    x, y = np.meshgrid(np.arange(window_size), np.arange(window_size))
    distance = np.sqrt((x - window_size // 2)**2 + (y - window_size // 2)**2)
    weight = 1.0 / (1.0 + distance**1.5)  # Weighted distance
    threshold=np.quantile(weight,0.17)
    weight[weight<threshold]=0
    # Normalize the weights
    weight /= weight.sum()

    # Apply convolution with the weighted kernel
    smoothed_data = convolve(np.nan_to_num(data), weight, mode='constant', cval=0.0)
    return smoothed_data


def aggregate_by_40_max(input_array,fun):
    if fun=="max":
        small = input_array.reshape([int(input_array.shape[1]//40), 40,int(input_array.shape[1]//40), 40]).max(3).max(1)
    if fun=="min":
        small = input_array.reshape([int(input_array.shape[1]//40), 40,int(input_array.shape[1]//40), 40]).min(3).min(1)
    elif fun=="mean":
        small = input_array.reshape([int(input_array.shape[1]//40), 40,int(input_array.shape[1]//40), 40]).mean(3).mean(1)
    elif fun=="nanmean":
        small = np.nan_to_num(np.nanmean(np.nanmean(input_array.reshape([int(input_array.shape[1]//40), 40,int(input_array.shape[1]//40), 40]),axis=3),axis=1))
    elif fun=="sum":
        small = input_array.reshape([int(input_array.shape[1]//40), 40,int(input_array.shape[1]//40), 40]).sum(3).sum(1)
    return small

def fun_patchiness(input_array):
    output_array=np.zeros((input_array.shape[1]//40,input_array.shape[2]//40))
    for x in range(0,output_array.shape[0]):
        for y in range(0,output_array.shape[0]):
            output_array[x,y]=label(input_array[0,(40*x):(40*x+40),(40*y):(40*y+40)])[1]
    return output_array




def process_geotiff(input_file, output_file,relative_date,num_windows,groundtruth1m_called,groundtruth3m_called,groundtruth6m_called,groundtruth12m_called):
    # Open the GeoTIFF file
    with rasterio.open(input_file) as src:
        newtransform=src.transform*src.transform.scale(40,40)
        # Get the dimensions of the raster
        width = src.width
        height = src.height

        # Calculate the number of windows (4 equal parts)
        window_width = width // 2
        window_height = height // 2
        latest_deforestation_file=output_file.replace("layer","timesinceloss")
        create_latest_deforestation= not os.path.isfile(latest_deforestation_file)
        threemonths_file=output_file.replace("layer","lastthreemonths")
        create_threemonths = not os.path.isfile(threemonths_file)
        sixmonths_file=output_file.replace("layer","lastsixmonths")
        create_sixmonths = not os.path.isfile(sixmonths_file) 
        twelvetosixmonths_file=output_file.replace("layer","previoussameseason")
        create_twelvetosixmonths = not os.path.isfile(twelvetosixmonths_file)
        totaldeforestation_file=output_file.replace("layer","totallossalerts")
        create_totaldeforestation = not os.path.isfile(totaldeforestation_file)
        groundtruth1m_file=output_file.replace("layer","groundtruth1m").replace("input","groundtruth")
        create_groundtruth1m = not os.path.isfile(groundtruth1m_file) and groundtruth1m_called==1
        groundtruth3m_file=output_file.replace("layer","groundtruth3m").replace("input","groundtruth")
        create_groundtruth3m = not os.path.isfile(groundtruth3m_file) and groundtruth3m_called==1
        groundtruth6m_file=output_file.replace("layer","groundtruth6m").replace("input","groundtruth")
        create_groundtruth6m = not os.path.isfile(groundtruth6m_file) and groundtruth6m_called==1
        groundtruth12m_file=output_file.replace("layer","groundtruth12m").replace("input","groundtruth")
        create_groundtruth12m = not os.path.isfile(groundtruth12m_file) and groundtruth12m_called==1
        confidence_file=output_file.replace("layer","confidence")
        create_confidence = not os.path.isfile(confidence_file)
        patchiness_file=output_file.replace("layer","patchdensity")
        create_patchiness = not os.path.isfile(patchiness_file)
        smoothedtotal_file=output_file.replace("layer","smoothedtotal")
        create_smoothedtotal = not os.path.isfile(smoothedtotal_file)
        smoothedsixmonths_file=output_file.replace("layer","smoothedsixmonths")
        create_smoothedsixmonths = not os.path.isfile(smoothedsixmonths_file)
        lastmonth_file=output_file.replace("layer","lastmonth")
        create_lastmonth = not os.path.isfile(lastmonth_file)
        # Iterate over windows
        if any([create_confidence,create_groundtruth1m,create_groundtruth3m,create_groundtruth6m,create_groundtruth12m,create_totaldeforestation,create_sixmonths,create_threemonths,
            create_twelvetosixmonths,create_latest_deforestation,create_patchiness,create_smoothedtotal,create_smoothedsixmonths,create_lastmonth,create_groundtruth12m,create_groundtruth1m,create_groundtruth3m,create_groundtruth6m]):
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
                    if create_latest_deforestation: latest_deforestation=template.copy()
                    if create_threemonths: threemonths=template.copy()
                    if create_sixmonths or create_smoothedsixmonths: sixmonths=template.copy()
                    if create_twelvetosixmonths: twelvetosixmonths=template.copy()
                    if create_totaldeforestation or create_smoothedtotal: totaldeforestation=template.copy()
                    if create_confidence: confidence=template.copy()
                    if create_patchiness: patchiness=template.copy()
                    if create_lastmonth: lastmonth=template.copy()
                    if create_groundtruth1m: groundtruth1m=template.copy()
                    if create_groundtruth3m: groundtruth3m=template.copy()
                    if create_groundtruth6m: groundtruth6m=template.copy()
                    if create_groundtruth12m: groundtruth12m=template.copy()
                offx2=(offx1+(template.shape[0]//2))
                offy2=(offy1+(template.shape[1]//2))


                # Take the remainder of 10000 for every pixel
                if create_confidence: confidence[offx1:offx2,offy1:offy2]=aggregate_by_40_max((np.remainder(np.nan_to_num(data), 10000)<relative_date).astype(int)*data//10000,fun="nanmean")
                data = np.remainder(np.nan_to_num(data), 10000)
                if create_groundtruth1m: 
                    groundtruth1m[offx1:offx2,offy1:offy2]=aggregate_by_40_max(((data<=(relative_date+30))&(data>relative_date)).astype(int),fun="sum")
                if create_groundtruth3m: 
                    groundtruth3m[offx1:offx2,offy1:offy2]=aggregate_by_40_max(((data<=(relative_date+61))&(data>relative_date)).astype(int),fun="sum")
                if create_groundtruth6m: 
                    groundtruth6m[offx1:offx2,offy1:offy2]=aggregate_by_40_max(((data<=(relative_date+182))&(data>relative_date)).astype(int),fun="sum")
                if create_groundtruth12m: 
                    groundtruth12m[offx1:offx2,offy1:offy2]=aggregate_by_40_max(((data<=(relative_date+365))&(data>relative_date)).astype(int),fun="sum")
                #remove all future data for the next features
                data[data>relative_date]=0


                #remove current date from data to get relative date, ignoring 0's, then aggregate by 40.     
                if create_latest_deforestation: latest_deforestation[offx1:offx2,offy1:offy2]=aggregate_by_40_max(np.multiply(np.divide(data,relative_date),10000).astype(int),fun="max")

                #remove current date from data to get relative date, ignoring 0's, then aggregate by 40.  
                if create_lastmonth: lastmonth[offx1:offx2,offy1:offy2]=aggregate_by_40_max((data>(relative_date-30)).astype(int),fun="sum")
                if create_threemonths: threemonths[offx1:offx2,offy1:offy2]=aggregate_by_40_max((data>(relative_date-92)).astype(int),fun="sum")
                if create_sixmonths or create_smoothedsixmonths: sixmonths[offx1:offx2,offy1:offy2]=aggregate_by_40_max((data>(relative_date-183)).astype(int),fun="sum")
                #for now patchiness uses 6 months as well.
                if create_patchiness: patchiness[offx1:offx2,offy1:offy2]=fun_patchiness((data>(relative_date-183)).astype(int))
                if create_twelvetosixmonths: twelvetosixmonths[offx1:offx2,offy1:offy2]=aggregate_by_40_max(((data<=(relative_date-183))&(data>(relative_date-366))).astype(int),fun="sum")
                if create_totaldeforestation or create_smoothedtotal: totaldeforestation[offx1:offx2,offy1:offy2]=aggregate_by_40_max((data>0).astype(int),fun="sum")

                   
            if create_latest_deforestation:
                with rasterio.open(latest_deforestation_file, 'w', driver='GTiff',compress='LZW', width=width//40, height=height//40, count=1, dtype=src.dtypes[0], crs=src.crs, transform=newtransform) as dst:
                    dst.write(latest_deforestation.reshape(1,latest_deforestation.shape[0],latest_deforestation.shape[1]))


            if create_threemonths:
                with rasterio.open(threemonths_file, 'w', driver='GTiff',compress='LZW', width=width//40, height=height//40, count=1, dtype=src.dtypes[0], crs=src.crs, transform=newtransform) as dst:
                    dst.write(threemonths.reshape(1,threemonths.shape[0],threemonths.shape[1]))

            
            if create_sixmonths:
                with rasterio.open(sixmonths_file, 'w', driver='GTiff',compress='LZW', width=width//40, height=height//40, count=1, dtype=src.dtypes[0], crs=src.crs, transform=newtransform) as dst:
                    dst.write(sixmonths.reshape(1,sixmonths.shape[0],sixmonths.shape[1]))

            
            if create_twelvetosixmonths:
                with rasterio.open(twelvetosixmonths_file, 'w', driver='GTiff',compress='LZW', width=width//40, height=height//40, count=1, dtype=src.dtypes[0], crs=src.crs, transform=newtransform) as dst:
                    dst.write(twelvetosixmonths.reshape(1,twelvetosixmonths.shape[0],twelvetosixmonths.shape[1]))

            
            if create_totaldeforestation:
                with rasterio.open(totaldeforestation_file, 'w', driver='GTiff',compress='LZW', width=width//40, height=height//40, count=1, dtype=src.dtypes[0], crs=src.crs, transform=newtransform) as dst:
                    dst.write(totaldeforestation.reshape(1,totaldeforestation.shape[0],totaldeforestation.shape[1]))
            
            if create_confidence:
                with rasterio.open(confidence_file, 'w', driver='GTiff',compress='LZW', width=width//40, height=height//40, count=1, dtype="float32", crs=src.crs, transform=newtransform) as dst:
                    dst.write(confidence.reshape(1,confidence.shape[0],confidence.shape[1]))

            if create_patchiness:
                with rasterio.open(patchiness_file, 'w', driver='GTiff',compress='LZW', width=width//40, height=height//40, count=1, dtype="uint16", crs=src.crs, transform=newtransform) as dst:
                    dst.write(patchiness.reshape(1,patchiness.shape[0],patchiness.shape[1]))

            if create_smoothedtotal:
                smoothedtotal=weighted_smoothing(totaldeforestation, window_size=31)
                with rasterio.open(smoothedtotal_file, 'w', driver='GTiff',compress='LZW', width=width//40, height=height//40, count=1, dtype="uint16", crs=src.crs, transform=newtransform) as dst:
                    dst.write(smoothedtotal.reshape(1,smoothedtotal.shape[0],smoothedtotal.shape[1]))

            if create_smoothedsixmonths:
                smoothedsixmonths=weighted_smoothing(sixmonths, window_size=31)
                with rasterio.open(smoothedsixmonths_file, 'w', driver='GTiff',compress='LZW', width=width//40, height=height//40, count=1, dtype="uint16", crs=src.crs, transform=newtransform) as dst:
                    dst.write(smoothedsixmonths.reshape(1,smoothedsixmonths.shape[0],smoothedsixmonths.shape[1]))
            
            if create_lastmonth:
                with rasterio.open(lastmonth_file, 'w', driver='GTiff',compress='LZW', width=width//40, height=height//40, count=1, dtype="uint16", crs=src.crs, transform=newtransform) as dst:
                    dst.write(lastmonth.reshape(1,lastmonth.shape[0],lastmonth.shape[1]))

            if create_groundtruth1m:
                with rasterio.open(groundtruth1m_file, 'w', driver='GTiff',compress='LZW', width=width//40, height=height//40, count=1, dtype="uint16", crs=src.crs, transform=newtransform) as dst:
                    dst.write(groundtruth1m.reshape(1,groundtruth1m.shape[0],groundtruth1m.shape[1]))
                    
            if create_groundtruth3m:
                with rasterio.open(groundtruth3m_file, 'w', driver='GTiff',compress='LZW', width=width//40, height=height//40, count=1, dtype="uint16", crs=src.crs, transform=newtransform) as dst:
                    dst.write(groundtruth3m.reshape(1,groundtruth3m.shape[0],groundtruth3m.shape[1]))

            if create_groundtruth6m:
                with rasterio.open(groundtruth6m_file, 'w', driver='GTiff',compress='LZW', width=width//40, height=height//40, count=1, dtype="uint16", crs=src.crs, transform=newtransform) as dst:
                    dst.write(groundtruth6m.reshape(1,groundtruth6m.shape[0],groundtruth6m.shape[1]))

            if create_groundtruth12m:
                with rasterio.open(groundtruth12m_file, 'w', driver='GTiff',compress='LZW', width=width//40, height=height//40, count=1, dtype="uint16", crs=src.crs, transform=newtransform) as dst:
                    dst.write(groundtruth12m.reshape(1,groundtruth12m.shape[0],groundtruth12m.shape[1]))


if __name__ == "__main__":
    # Create a command-line argument parser
    parser = argparse.ArgumentParser(description="Apply calculation to a geotiff image.")
    parser.add_argument("input_image", help="Path to the input geotiff image")
    parser.add_argument("output_image", help="Path to the output geotiff image")
    parser.add_argument("relative_date", help="relative date")
    parser.add_argument("--groundtruth1m", help="should groundtruth1m be processed",default=1,required=False)
    parser.add_argument("--groundtruth3m", help="should groundtruth3m be processed",default=1,required=False)
    parser.add_argument("--groundtruth6m", help="should groundtruth6m be processed",default=1,required=False)
    parser.add_argument("--groundtruth12m", help="should groundtruth12m be processed",default=1,required=False)
    parser.add_argument("--num_windows", help="number of windows, depends on RAM size.",default=4,required=False)
    args = parser.parse_args()
    # Replace 'your_geotiff_file.tif' with the actual file path
    input_geotiff =  args.input_image
    output_geotiff = args.output_image
    reldate=int(args.relative_date)
    num_windows=int(args.num_windows)
    process_geotiff(input_geotiff,output_geotiff,reldate,num_windows = num_windows,
        groundtruth1m_called=int(args.groundtruth1m),groundtruth3m_called=int(args.groundtruth3m),groundtruth6m_called=int(args.groundtruth6m),groundtruth12m_called=int(args.groundtruth12m))