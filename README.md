
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
| `train_predict_raster` | Combines the above functions as a wrapper to do everything you need related to ForestForesight |

## Package data
The data below can be loaded using 
    
    data({feature})
    {feature}=vect({feature}) #to turn the feature into the nominally used terra format (from saved sf format)

| feature | usage     |
| :-------- | :------- |
| `gfw_tiles` | a spatial polygon dataset of all GFW tiles (103) of 10x10 degrees covering the pantropical belt|
| `countries` | a spatial polygon dataset of all countries with ISO3 codes. This also contains the grouping of countries how we currently process them|
| `degree_polygons` | Used mostly for data analysis, these are 1x1 degree polygons intersected by country, used in the powerBI analysis dashboard|



For detailed usage instructions and function documentation, please refer to the package documentation or use ?function_name in R.
## Installation

Install in R (at least 4.3.1) with 

    devtools::install_github("jduijvenbode-WWF/ForestForesight")

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

## Features
[At the moment we have the following features](https://github.com/jduijvenbode-WWF/ForestForesight/blob/main/Feature%20metadata.xlsx)

## FAQ

### Do I need to build the models myself?
No, as long as you also use the data as created by ForestForesight of WWF-NL on the resource bucket you can use the pre-existing models from there as well. you can continue training with it or use the models as is.


### Where can I download the preprocessed data?

We put all the data per 10x10 degree tile on an S3 server. You can access the data by using the AWS CLI or a client like Cyberduck. We recommend the latter. For this you first need to download cyberduck and when installed press "Open connection" in the top left. Then the following settings are required:
![cyberduck instructions](https://github.com/jduijvenbode-WWF/ForestForesight/blob/main/cyberduck_instructions.png)

 ### Can I add my own features?
 
 Absolutely. We strongly encourage you to do so in your own models. The only requirements of the output of your preprocessing are:
- all the data has the same coordinate system (We use WGS84 lat/long, AKA EPSG:4326)
- The data is stored per tile/folder (ff_prep uses this data structure to get the right data) where the name of the folder follows the structure YY{N/S}_XXX{W/E}, for example *10N_080W*
- The datasets are in geotiff format (without WLD-file, CRS etc should be in geotiff header)
- The naming of the geotiffs follow the structure YY{N/S}_XXX{W/E}_yyyy-mm-dd_{featurename}.tif, for example *10N_080W_2021-01-01_slope.tif*
- The date in the filename should always be the first of the month, the maximum temporal resolution of ForestForesight is monthly (predicting six months ahead)
- The data should always have per tile and per month at least 1 dataset for groundtruth if you want to train and/or analyse the score of your model/predictions.

## Feature Wishlist & Roadmap

- Using categorical data. This is currently not working in R on XGboost and we would like to implement the one-hot encoding for this at some point.
- Develop the processing chain in python.
- Implementing our somewhat better performing deep learning algorithm
- Predicting the actual amount of expected deforestation in a hotzone. Currently we are hitting a correlation coefficient of 0.38 compared to only 0.11 as a correlation between binary certainty (precision/recall).
- Smart grouping of predictions so that it is easier for users to go to areas. There is however a very large non-technical discussion required to make this step. Feel free if this has your interest to get in touch.
- Automatic updating of our prediction dashboards through ArcGIS Online API
  
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

