library(ForestForesight)
data("gfw_tiles")
cov=terra::aggregate(vect(gfw_tiles))
shp1=vect("C:/data/WDPA_Jan2024_Public_shp-polygons1.shp",filter=cov)

shp1=shp1[cov]
shp1=aggregate(shp1)
shp1=intersect(shp1,cov)[[1]]
shp1s=simplifyGeom(shp1,200/110000)

shp2=vect("C:/data/WDPA_Jan2024_Public_shp-polygons2.shp",filter=cov)
shp2=shp2[cov]
shp2=aggregate(shp2)
shp2=intersect(shp2,cov)[[1]]
shp2s=simplifyGeom(shp2,200/110000)

shp3=vect("C:/data/WDPA_Jan2024_Public_shp-polygons3.shp",filter=cov)
shp3=shp3[cov]
shp3=aggregate(shp3)
shp3=intersect(shp3,cov)[[1]]
shp3s=simplifyGeom(shp3,200/110000)

allshp=rbind(rbind(shp1s,shp2s),shp3s)
writeVector(allshp2,"C:/data/storage/contextualization/WDPAcomb2.json")
data("countries")
countries=vect(countries)
allshp2=intersect(allshp,countries)
for(rasje in tempras){
  tras=rast(rasje)
  result=rasterize(crop(allshp2,ext(tras)),tras,background=0)*(tras>0)
  writeRaster(result,gsub("landpercentage","wdpa",rasje),datatype="INT1U",gdal="COMPRESS = DEFLATE")
}
