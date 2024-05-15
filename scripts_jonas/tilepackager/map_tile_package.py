import argparse
import os

class ResourceNotFoundError(Exception):
    pass

class RasterNotExistError(Exception):
    pass

class RasterNotSupportedError(Exception):
    pass

class TemplateNotFoundError(Exception):
    pass

ws = os.path.dirname(__file__)
template = os.path.join(ws, r'temps\template.aprx')

temp_lyr = os.path.join(ws, r"temps\lyr.lyrx")
if not os.path.exists(template) or not os.path.exists(temp_lyr):
    raise TemplateNotFoundError('Template Not Found (folder named temps in same location as python script)')
    
parser = argparse.ArgumentParser(description='Map tile package script')
parser.add_argument('image', type=str, help='Input image, should a single band image')
parser.add_argument('output', type=str, help='Output map tile')


args = parser.parse_args()
image = args.image
output = args.output

dirname = os.path.dirname(output)
basename = os.path.basename(output)

if not os.path.exists(dirname):
    raise ResourceNotFoundError("Output folder does not exist")
    
import arcpy


def is_single_band_continuous_image(raster_path):
    if not arcpy.Exists(raster_path):
        raise RasterNotExistError("Raster dataset does not exist.")
        
    try:
        raster = arcpy.Raster(raster_path)
        # Check if the raster has only one band
        if raster.bandCount == 1:
            # Check if the pixel type is continuous
            if raster.pixelType.lower() in ['f32', 'f64', 'u32', 's32', 'u16', 's16']:
                return True
    except Exception as e:
        raise RasterNotSupportedError('Rater Not Supported')
    return False

if not is_single_band_continuous_image(image):
    raise RasterNotSupportedError('Rater Not Supported')

project = arcpy.mp.ArcGISProject(template)
active_map = project.listMaps()[0]

# Add raster layer to the map
lyr = arcpy.management.MakeRasterLayer(image, 'image')[0]
lyr = arcpy.ApplySymbologyFromLayer_management(lyr, temp_lyr)[0]

print(lyr.symbology.colorizer.stretchType)
active_map.addLayer(lyr)

arcpy.management.CreateMapTilePackage(
    in_map=active_map,
    service_type="ONLINE",
    output_file=output,
    format_type="PNG8",
    level_of_detail=13,
    service_file=None,
    summary="Map Tile",
    tags="",
    extent="DEFAULT",
    compression_quality=75,
    package_type="tpkx",
    min_level_of_detail=0,
    create_multiple_packages="CREATE_SINGLE_PACKAGE",
)