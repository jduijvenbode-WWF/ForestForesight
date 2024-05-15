
![Logo](https://www.wwf.nl/globalassets/afbeeldingen/projecten/forest-foresight/logo-forest-foresight.jpg?mode=crop&autorotate=true&upscale=False&width=1110&format=webp)


## Author
- [@Jonas van Duijvenbode](https://www.linkedin.com/in/jonas-van-duijvenbode-576a1b43?original_referer=https%3A%2F%2Fwww%2Egoogle%2Ecom%2F&originalSubdomain=nl)

This R package provides tools for generating 6-month deforestation predictions, facilitating analysis at various spatial scales. The predictions are based on data divided into 10x10 degree tiles and can be conducted at the country, tile, or group-of-tiles level. It leverages both user-provided data and preprocessed datasets available on an open S3 bucket, covering the entire land-covered pantropical belt between -30 and 30 degrees latitude. The system makes for our global model extensive use of the GlobalForestWatch Integrated Alerts, which can be found [here](https://www.globalforestwatch.org/blog/data-and-research/integrated-deforestation-alerts/).

![Logo](https://content.globalforestwatch.org/wp-content/uploads/2021/12/integrated-deforestation-alerts-layer-2021.png)

## Functions
| function | usage     |
| :-------- | :------- |
| `ff_dqc` | checks data completeness prior to data preparation (not required)|
| `ff_prep` | Prepare and load the data into memory from geotiffs to a matrix|
| `ff_train` | Use XGBoost to train a model based on training data and labels|
| `ff_predict` | Predict deforestation with the trained model and new data input |
| `ff_analyse` | Analyses the predictions for accuracy (optional: output to powerbi)|
| `visualize` | outputs the predictions to a vector file|

## Package data
The data below can be loaded using 
    
    data({name of dataset})
    {feature}=vect({feature}) #to turn the feature into the nominally used terra format (from saved sf format)

| function | usage     |
| :-------- | :------- |
| `gfw_tiles` | a spatial polygon dataset of all GFW tiles (103) of 10x10 degrees covering the pantropical belt|
| `countries` | a spatial polygon dataset of all countries with ISO3 codes|
| `degree_polygons` | Used mostly for data analysis, these are 1x1 degree polygons intersected by country|



For detailed usage instructions and function documentation, please refer to the package documentation or use ?function_name in R.
## Installation

Install in R (at least 4.3.1) with 

    devtools::install_github("jduijvenbode-WWF/ForestForesight/package/ForestForesight")

Load the package with
    
    library(ForestForesight)

The package requires the following packages:
* terra
* lubridate
* sf
* xgboost
* httr

## Data preparation
We strongly suggest starting with our preprocessed data which can be downloaded [here](https://s3.console.aws.amazon.com/s3/buckets/wwf-ff-global?region=eu-west-1). Use [Cyberduck](https://cyberduck.io) or your other preferred S3-client to download the tiles you want.

The S3 bucket contains multiple folders which are structured like this
![Folder overview](https://github.com/jduijvenbode-WWF/ForestForesight/blob/main/data%20structure.jpg)


At the moment we have the following features:
|feature|name|periodicity|source|processing|max DN|
| :-------- | :------- | :-------- | :------- | :-------- | :-------- | 
|groundtruth6mbin|binary groundtruth of oncoming six months|monthly|[GFW alerts](https://www.globalforestwatch.org/blog/data-and-research/integrated-deforestation-alerts/)|every 400x400m pixel where at least 1 deforestation event happens is classified as an actual (value 1)|1|
|groundtruth1m|groundtruth of oncoming month in amount of pixels|monthly|[GFW alerts](https://www.globalforestwatch.org/blog/data-and-research/integrated-deforestation-alerts/)|every 400x400m pixel with the total number of deforestation events in the oncoming 1 months as a value (betwen 0 and 1600)|1600|
|groundtruth3m|groundtruth of oncoming 3 months in amount of pixels|monthly|[GFW alerts](https://www.globalforestwatch.org/blog/data-and-research/integrated-deforestation-alerts/)|every 400x400m pixel with the total number of deforestation events in the oncoming 3 months as a value (betwen 0 and 1600)|1600|
|groundtruth6m|groundtruth of oncoming 6 months in amount of pixels|monthly|[GFW alerts](https://www.globalforestwatch.org/blog/data-and-research/integrated-deforestation-alerts/)|every 400x400m pixel with the total number of deforestation events in the oncoming 6 months as a value (betwen 0 and 1600)|1600|
|groundtruth12m|groundtruth of oncoming 12 months in amount of pixels|monthly|[GFW alerts](https://www.globalforestwatch.org/blog/data-and-research/integrated-deforestation-alerts/)|every 400x400m pixel with the total number of deforestation events in the oncoming 12 months as a value (betwen 0 and 1600)|1600|
|closenesstoroads|closeness to roads|yearly|[OSM](https://wiki.openstreetmap.org/wiki/Downloading_data)|This is downloaded using the ohsome package with the filter type:way and highway=* and geometry:line and then further filtered on only lines. The lines are simplified on 10 meters. then the distance in 400x400m pixels is calculated in python. the distance is then converted by the formula 255-20*log(distance+1)|255|
|closenesstowaterways|closeness to waterways|static|[OSM](https://wiki.openstreetmap.org/wiki/Downloading_data)|This is downloaded using the ohsome package with the filter type:way and highway=* and geometry:line and then further filtered on only lines. The lines are simplified on 10 meters. then the distance in 400x400m pixels is calculated in python. the distance is then converted by the formula 255-20*log(distance+1)|255|
|confidence|average confidence of alerts|monthly|[GFW alerts](https://www.globalforestwatch.org/blog/data-and-research/integrated-deforestation-alerts/)|The average confidence of the alerts, which are between 2 (low confidence, detected by one EWS once) and 4 (highest confidence, detected by mutiple EWS multiple times) meaning low to highest confidence with NA (not a number) removed|4|
|elevation|elevation|static|[SRTM](https://csidotinfo.wordpress.com/data/srtm-90m-digital-elevation-database-v4-1/)|the average elevation of the SRTM rasters when aggregated to the 400x400m pixel level in meters above sea level|8849|
|firealerts|forest fire alerts from VIIRS and other satellites|monthly|[VIIRS LAADS-DAAP](https://firms.modaps.eosdis.nasa.gov/download/)|the data is downloaded for all sensors for historic data and from now on monthly for near real time data. The point shapefiles are rasterized with the sum of all points within the pixel. Thus the number represents number of forest fires within the pixel in the last six months|1000|
|forestheight|height of forest 2020|yearly|[GLAD](https://glad.umd.edu/users/Potapov/GLCLUC2020/Forest_height_2020/)|this dataset is available till 2020 and represents the average forest height in meters within the pixel, where 0 means no forest|50|
|historicloss|GLAD deforestation 2001-2018|static|[GLAD FSC alerts](https://glad-forest-alert.appspot.com/)|The total number of pixels between 2001 and 2018 being deforested according to the yearly report|256|
|initialforestcover|forest mask of 2019|static|[GLAD FSC alerts](https://glad-forest-alert.appspot.com/)|The sum of the underlying pixels in the 400x400m pixel where every underlying pixel is a fraction of canopy cover and every pixel that has been deforested between 2001 and 2018 is set to 0|10000|
|lastsixmonths|deforestation last six months|monthly|[GFW alerts](https://www.globalforestwatch.org/blog/data-and-research/integrated-deforestation-alerts/)|takes the sum of the underlying 1600 10x10m pixels that have been deforested in the last 6 months|1600|
|lastthreemonths|deforestation last three months|monthly|[GFW alerts](https://www.globalforestwatch.org/blog/data-and-research/integrated-deforestation-alerts/)|takes the sum of the underlying 1600 10x10m pixels that have been deforested in the last 3 months|1600|
|lastmonth|deforestation in the previous month|monthly|[GFW alerts](https://www.globalforestwatch.org/blog/data-and-research/integrated-deforestation-alerts/)|takes the sum of the underlying 1600 10x10m pixels that have been deforested in the previous month|1600|
|losslastyear|GLAD deforestation last year|yearly|[GLAD FSC alerts](https://glad-forest-alert.appspot.com/)|The total number of underlying pixels as a sum from the previous year|256|
|month|the number of the month|static|autogenerated|this is autogenerated in the preprocessing|12|
|nightlights|nighttime activity as measured by the VIIRS satellite|monthly|[VIIRS LAADS-DAAP](https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/5000/VNP46A3/2023)|monthly cloud-corrected nighttime activity (visible light/SWIR radiation at night) where the values represent the amount of radiation|65535|
|patchdensity|patchiness of last six months of deforestation|monthly|[GFW alerts](https://www.globalforestwatch.org/blog/data-and-research/integrated-deforestation-alerts/)|the deforestation of the last six months is taken as input and processed by giving every group of deforestation pixels a unique value. The total amount of unique values within the 400x400m pixel is the result, representing the amount of deforestation patches in the last 6 months|800|
|totaldeforestation|the total number of deforested pixels according to the integrated alerts|monthly|[GFW alerts](https://www.globalforestwatch.org/blog/data-and-research/integrated-deforestation-alerts/)|takes the sum of the underlying 1600 10x10m pixels that have been deforested since the beginning of the EWS (normally 1st of january 2020)|1600|
|peatland|areas with a peat soil|static|[CIFOR](https://data.cifor.org/dataset.xhtml?persistentId=doi:10.17528/CIFOR/DATA.00058)|the data is aggregated to 400x400 and represents the average depth of the peat soil in meters with a maximum of 15 meters|15|
|populationcurrent|current population|static|[EUPOP](https://ghsl.jrc.ec.europa.eu/download.php)|This is the POP2025 (GHS-POP) dataset that is first reprojected to the 400x400m pixel resolution with the max function and then a gradual filter of 25 pixels is applied to smooth it over a larger area|20000000|
|populationincrease|population increase|static|[EUPOP](https://ghsl.jrc.ec.europa.eu/download.php)|This is the difference between POP2030 (the expected population in 2030, see source GHS-POP) and POP2020 (the known population according to census in 2020, see source GHS-POP) dataset that is first reprojected to the 400x400m pixel resolution with the max function and then a gradual filter of 25 pixels is applied to smooth it over a larger area|20000000|
|precipitation|predicted precipitation |monthly|[CMIP6](https://cds.climate.copernicus.eu/cdsapp#!/dataset/projections-cmip6-decadal-prototype?tab=form)|Averaged (median) over all 16 scenarios and with a cubic resampling resampled to 400x400m pixels. The actual values represent the average (mean) amount of expected precipitation in the 6 months following the date of the dataset in mm/month multiplied by 1e6|240|
|temperature|predicted temperature|monthly|[CMIP6](https://cds.climate.copernicus.eu/cdsapp#!/dataset/projections-cmip6-decadal-prototype?tab=form)|Averaged (median) over all 16 scenarios and with a cubic resampling resampled to 400x400m pixels. The actual values represent the average temperature in the 6 months following the date of the dataset in kelvin multiplied by 10 |3000|
|previoussameseason|deforestation 6-12 months ago|monthly|[GFW alerts](https://www.globalforestwatch.org/blog/data-and-research/integrated-deforestation-alerts/)|takes the sum of the underlying 1600 pixels that have been deforested 6-12 months to account for seasonality|1600|
|sinmonth|the sine of the date|static|autogenerated|this is autogenerated in the preprocessing and represents the sine value of the day of the year, between -1 and 1|1|
|slope|slope|static|[SRTM](https://csidotinfo.wordpress.com/data/srtm-90m-digital-elevation-database-v4-1/)|Slope is calculated using the terrain function in terra using standard settings and then taken the average of to aggregate|4000|
|smoothedsixmonths|deforestation last six months smoothed|monthly|[GFW alerts](https://www.globalforestwatch.org/blog/data-and-research/integrated-deforestation-alerts/)|A smoothed version of layer lastsixmonths. This means that the further away from the aggregated 400x400m pixel the pixel in question is, the lower the value gets.|1600|
|smoothedtotal|total deforestation smoothed|monthly|[GFW alerts](https://www.globalforestwatch.org/blog/data-and-research/integrated-deforestation-alerts/)|A smoothed version of layer totaldeforestation (the total number of deforested pixels according to the integrated alerts). This means that the further away from the aggregated 400x400m pixel the pixel in question is, the lower the value gets.|1600|
|timesinceloss|latest moment of deforestation|monthly|[GFW alerts](https://www.globalforestwatch.org/blog/data-and-research/integrated-deforestation-alerts/)|This is classified as highest value (representing latest moment of deforestation) since 1-1-2015 divided by the current date multiplied by 10.000 to make it 16-bit |10000|
|totallossalerts|total deforestation|monthly|[GFW alerts](https://www.globalforestwatch.org/blog/data-and-research/integrated-deforestation-alerts/)|The total amount of the underlying 1600 pixels deforested according to the GFW alerts since the alerts started|1600|
|wetlands|areas with wetlands|static|[FAO/WWF](https://www.worldwildlife.org/publications/global-lakes-and-wetlands-database-lakes-and-wetlands-grid-level-3)|the data is aggregated to 400x400 and reclassified from categorical values to 1/0 for wetland/no wetland by taking any value above 0|1|
|x|latitude |static|autogenerated|this is autogenerated in the preprocessing and represents the latitude per pixel between -90 and 90|90|
|y|longitude|static|autogenerated|this is autogenerated in the preprocessing  and represents the longitude per pixel between -180 and 180|180|
|monthssince2019|number of months since januari 2019|static|autogenerated|this is autogenerated in the preprocessing  and represents the amount of months since the first of january 2019|50|
|landpercentage|percentage of land cover (as opposed to sea)|static|[GADM](https://gadm.org)|this is the percentage landcover in values from 0 (no land) to 255 (100% landcover of the pixel)|255|
|catexcap|cation exchange capacity at 0-5cm|static|[ISRIC](https://data.isric.org/geonetwork/srv/eng/catalog.search#/metadata/867431df-9c35-4aaa-8cd7-93ee0226eb3d)|the data was aggregated to 400x400 meter by using the mode of the underlying pixels (most occurring). The cation exchange capacity in mmol(c)/kg is a useful indicator for soil fertility, a proxy for deforestation for agriculture|1000|
|wdpa|WDPA status|yearly|[Protected Planet](https://www.protectedplanet.net/en/thematic-areas/wdpa)|the data was intersected with land area and the tiles that we have, then simplified to 400 meters and then rasterized. A value of 1 means that the entire area is protected area, 0 means no protected area within the pixel|1|
|croplandcapacity100p|Agricultural conversion capacity (cropland only)|static|[RIBES](https://zenodo.org/records/7665902)|the data was reprojected to 400x400m by using a nearest neighbor resampling.|255|
|croplandcapacitybelow50p|Agricultural conversion capacity (less than 50 percent cropland cover)|static|[RIBES](https://zenodo.org/records/7665902)|the data was reprojected to 400x400m by using a nearest neighbor resampling.|255|
|croplandcapacityover50p|Agricultural conversion capacity (more than 50 percent cropland cover)|static|[RIBES](https://zenodo.org/records/7665902)|the data was reprojected to 400x400m by using a nearest neighbor resampling.|255|
[^1]overview of the datasets of Forest Foresight


    
## FAQ

### Do I need to build the models myself?
No, as long as you also use the data as created by ForestForesight of WWF-NL on the resource bucket you can use the pre-existing models from there as well. you can continue training with it or use the models as is.


### Where can I download the preprocessed data?

We put all the data per 10x10 degree tile on an S3 server, which you can find
 [here](https://s3.console.aws.amazon.com/s3/buckets/wwf-ff-global?region=eu-west-1)

 ### Can I add my own features?
 
 Absolutely. We strongly encourage you to do so in your own models. The only requirements of the output of your preprocessing are:
- all the data has the same coordinate system (We use WGS84 lat/long, AKA EPSG:4326)
- The data is stored per tile/folder (ff_prep uses this data structure to get the right data) where the name of the folder follows the structure YY{N/S}_XXX{W/E}, for example *10N_080W*
- The datasets are in geotiff format (without WLD-file, CRS etc should be in geotiff header)
- The naming of the geotiffs follow the structure YY{N/S}_XXX{W/E}_yyyy-mm-dd_{featurename}.tif, for example *10N_080W_2021-01-01_slope.tif*
- The date in the filename should always be the first of the month, the maximum temporal resolution of ForestForesight is monthly (predicting six months ahead)
- The data should always have per tile and per month at least 1 dataset for groundtruth if you want to train and/or analyse the score of your model/predictions.

## Feature Wishlist & Roadmap

- Addition of more model features that are a proxy for deforestation drivers.
- Improved model awareness of seasonality. We have thus far used expected precipitation and the current month and sine of the month to mitigate for seasonal trends but feel that this could still be better
- Model awareness of trends. The model relies on the same number of deforestation events occurring in the future. If this would increase or decrease then either the model should take this into account or the classification threshold should be changed accordingly. We have nothing for this yet.
- Using categorical data. This is currently not working in R on XGboost and we would like to implement the one-hot encoding for this
- Develop the processing chain in python.
- Either a separate model or a different classification for new deforestation, meaning in hotzones where no previous deforestation has occurred.
- Testing of algorithms other than XGBoost
- Usefulness of ForestForesight with different timespans in the future. For now we predict six months in the future but how useful would 2 months or 12 months be?
- An accuracy and accuracy metric for Forest Foresight as a continuous deforestation risk. For the moment we have thought of implementing the RMSE as a metric and comparing it to the baseline. Groundtruth should be changed to an amount for this.
- Smart grouping of predictions so that it is easier for users to go to areas. There is however a very large non-technical discussion required to make this step
- Automatic updating of our prediction dashboards through ArcGIS Online API
- Quality control scripts that check whether the Integrated Alerts have been sufficiently updated. We do not know at the moment what the last moment of update from the three underlying systems of the Integrated Alerts are.



## Contributing

Contributions are always welcome and encouraged! Help us predict and prevent deforestation and help the people using our predictions by making them better!

To find out how you can contribute, email jduijvenbode@wwf.nl

Please adhere to this project's `code of conduct`.
## License

[MIT](https://choosealicense.com/licenses/mit/)[![MIT License](https://img.shields.io/badge/License-MIT-green.svg)](https://choosealicense.com/licenses/mit/)


## Acknowledgements

 - [Jorn Dallinga](https://www.bing.com/ck/a?!&&p=12dd7504485fe85eJmltdHM9MTcwNjQ4NjQwMCZpZ3VpZD0zZWJlMjY2OC03YThkLTY1Y2EtMTk3MS0zNWRjN2JjNTY0MGQmaW5zaWQ9NTE5OA&ptn=3&ver=2&hsh=3&fclid=3ebe2668-7a8d-65ca-1971-35dc7bc5640d&psq=jorn+dallinga&u=a1aHR0cHM6Ly9ubC5saW5rZWRpbi5jb20vaW4vam9ybi1kYWxsaW5nYS01YjA5NmE5MA&ntb=1)
 - [Zillah Calle](https://www.bing.com/ck/a?!&&p=43f984ae65749bc4JmltdHM9MTcwNjQ4NjQwMCZpZ3VpZD0zZWJlMjY2OC03YThkLTY1Y2EtMTk3MS0zNWRjN2JjNTY0MGQmaW5zaWQ9NTE5NA&ptn=3&ver=2&hsh=3&fclid=3ebe2668-7a8d-65ca-1971-35dc7bc5640d&psq=zillah+calle&u=a1aHR0cHM6Ly9ubC5saW5rZWRpbi5jb20vaW4vemlsbGFoLWNhbGxlLTkwMDEzNTIzMQ&ntb=1)


## Support

For support, email jduijvenbode@wwf.nl


## Related

Find more information about Forest Foresight here:
[official page](https://www.wwf.nl/wat-we-doen/focus/bossen/forest-foresight)

