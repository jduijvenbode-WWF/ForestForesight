data("countries")
countries=vect(countries)
data("gfw_tiles")
gfw_tiles=vect(gfw_tiles)
indt=gfw_tiles[countries[which(countries$iso3=="IDN"),]]$tile_id

for(tile in indt){
  filename=file.path("D:/ff-dev/results/hotzones/tiles",tile,paste0(tile,"_hotzones.json"))
  filename_centers=file.path("D:/ff-dev/results/hotzones/tiles",tile,paste0(tile,"_hotzones_centers.json"))
  if(file.exists(filename)){
    hz=vect(filename)
    print(length(hz))
    hz=hz[which(hz$country=="Indonesia"),]

    if(length(hz)>0){if(!exists("allhz")){allhz=hz}else{allhz=rbind(allhz,hz)}}
  }
  if(file.exists(filename_centers)){
    hzc=vect(filename_centers)
    hzc=hzc[which(hzc$country=="Indonesia"),]
    if(length(hzc)>0){if(!exists("allhzc")){allhzc=hzc}else{allhzc=rbind(allhzc,hzc)}}
  }
}
allhz$ID=seq(length(allhz))
allhzc$ID=seq(length(allhzc))
allhzc=allhzc[which(allhzc$certainty>95),]
allhz=allhz[which(allhz$certainty>95),]

writeVector(allhz,"D:/ff-dev/results/hotzones/countries/indonesia2.gpkg",layer="hotzones",overwrite=T)
writeVector(allhzc,"D:/ff-dev/results/hotzones/countries/indonesia2.gpkg",layer="hotzone_centers",insert=T,overwrite=T)

