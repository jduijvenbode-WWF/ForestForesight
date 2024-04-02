library(ForestForesight)
data(countries)
setwd("C:/data")

country="Laos"
threshold=95
dir.create(country)
setwd(country)
shape <- terra::vect(countries)[which(countries$group==country),]
b=train_predict_raster(shape = shape,prediction_date = "2024-01-01",ff_folder="C:/data/storage",verbose=T)
polras=b
polras[polras<90]=NA
pols=as.polygons(polras,dissolve=F)
#########dashboard version 2########

gadm = vect("C:/Users/jonas/Downloads/gadm_410.gpkg")
gadm = gadm[which(gadm$COUNTRY == country), ]
gadm = gadm[, c("NAME_0", "NAME_1", "NAME_2", "NAME_3")]
ecobiome = vect("C:/data/storage/contextualization/ECOBIOME.gpkg",
                extent = ext(gadm))
wdpa = vect("C:/data/storage/contextualization/WDPA.gpkg",
            extent = ext(gadm))
ecobiome$ecobiome=paste(ecobiome$Biome,ecobiome$Ecoregion,sep="_")
wdpa2 = aggregate(wdpa)
wdpa = terra::intersect(wdpa, wdpa2)
nonwdpa = as.polygons(ext(wdpa)) - wdpa
nonwdpa$status = "not protected"
wdpa$status = "protected"
wdpa = rbind(wdpa, nonwdpa)
wdpa = aggregate(wdpa, by = "status")
wdpa = disagg(wdpa)

all = terra::intersect(gadm, ecobiome)
all2 = terra::intersect(all, wdpa)

colras = b
colras[colras<50]=NA
colrasproj=project(colras,"epsg:3857")
writeRaster(colrasproj,paste0(country,".tif"),overwrite=T)
all2proj=project(all2,"epsg:3857")
vals = extract(colras>0, all2proj, fun = "sum", na.rm = T)[, 2]
vals[is.na(vals)] = 0
highalertvals = extract(colras > threshold, all2, fun = "sum", na.rm = T)[, 2]
highalertvals[is.na(highalertvals)] = 0
all2$events = vals
all2$highalerts = highalertvals
all2$agg_n=NULL
names(all2) = c(
  "country",
  "province",
  "district",
  "municipality",
  "ecoregion",
  "biome",
  "ecobiome",
  "status",
  "events",
  "highalerts"
)
writeVector(centroids(all2), paste0(country,".overview.shp"), overwrite = T)

colras2 = colras
colras2[colras2 < threshold] = NA

pols = as.polygons(project(colras2,crs(all2)), dissolve = F)
polvals = extract(all2, centroids(pols))
polvals$events = NULL
polvals$highalerts = NULL
pols2 = cbind(pols, polvals)
writeVector(project(pols2,"epsg:3857"), paste0(country,"_highalerts.shp"))
#code to extract the high alert values and amount of normal alerts for wdpa, ecobiome and gadm separately
wdpa2 = terra::intersect(wdpa, aggregate(gadm))
eventcount=project(colras>0,crs(ecobiome))
wdpa2$events=extract(eventcount,wdpa2,fun="sum",na.rm=T)[,2]
wdpa2$events[which(is.na(wdpa2$events))]=0
writeVector(project(wdpa2,"epsg:3857"), paste0(country,"_wdpa.shp"),overwrite=T)

ecobiome2 = terra::intersect(ecobiome, aggregate(gadm))
ecobiome2$events=extract(eventcount,ecobiome2,fun="sum",na.rm=T)[,2]
ecobiome2$events[which(is.na(ecobiome2$events))]=0
writeVector(project(ecobiome2,"epsg:3857"), paste0(country,"_ecobiome.shp"),overwrite=T)
names(gadm)=c("country","province","district","municipality")

gadm$events=extract(eventcount,gadm,fun="sum",na.rm=T)[,2]
gadm$events[which(is.na(gadm$events))]=0
writeVector(project(gadm,"epsg:3857"), paste0(country,"_gadm.shp"),overwrite=T)
