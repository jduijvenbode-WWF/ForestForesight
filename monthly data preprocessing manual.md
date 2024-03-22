## monthly data preprocessing
at the moment there are three data sources that need to be updated monthly, being the Integrated Alerts, the forest fires and the nighttime activity. Later on this manual will be expanded to include yearly datasets like the hansen loss and distance to roads.

## integrated alerts
## download
to download the integrated alerts run the following lines:

```
#requires the package ForestForesight
library(ForestForesight)
data("gfw_tiles")
IA=vect(gfw_tiles)

setwd(dir.choose()) #for us this is the folder D:/ff-dev/alerts
for(id in seq(nrow(IA))){
  file=IA$download[id]
  name=IA$tile_id[id]
  b=httr::GET(file)
  writeBin(b$content,paste0(name,".tif"))
}
```