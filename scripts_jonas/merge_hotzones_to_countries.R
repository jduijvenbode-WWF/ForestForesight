data("countries")
countries=vect(countries)
per=countries[which(countries$iso3=="PER")]
data("gfw_tiles")
gfw_tiles=vect(gfw_tiles)
indt=gfw_tiles[countries[which(countries$iso3=="PER"),]]$tile_id

for(tile in indt){
  filename=file.path("D:/ff-dev/results/hotzones/tiles",tile,paste0(tile,"_hotzones.json"))
  filename_centers=file.path("D:/ff-dev/results/hotzones/tiles",tile,paste0(tile,"_hotzones_centers.json"))
  if(file.exists(filename)){
    hz=vect(filename)
    print(length(hz))
    hz=hz[which(hz$country=="Peru"),]
    hz$tile=tile
    if(length(hz)>0){if(!exists("allhz")){allhz=hz}else{allhz=rbind(allhz,hz)}}
  }
  if(file.exists(filename_centers)){
    hzc=vect(filename_centers)
    hzc=hzc[which(hzc$country=="Peru"),]
    hzc$tile=tile
    if(length(hzc)>0){if(!exists("allhzc")){allhzc=hzc}else{allhzc=rbind(allhzc,hzc)}}
  }
}
allhz$ID=seq(length(allhz))
allhzc$ID=seq(length(allhzc))
allhz=allhz[allhz$province=="Madre de Dios"]
allhzc=allhzc[allhzc$province=="Madre de Dios"]
# allhzc=allhzc[which(allhzc$certainty>75),]
# allhz=allhz[which(allhzc$certainty>75),]
allhzc[allhzc$province]
writeVector(allhz,"D:/ff-dev/results/hotzones/countries/peru.gpkg",layer="hotzones",overwrite=T)
writeVector(allhzc,"D:/ff-dev/results/hotzones/countries/peru.gpkg",layer="hotzone_centers",insert=T,overwrite=T)

