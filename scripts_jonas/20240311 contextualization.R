#########dashboard version 2########
library(ForestForesight)
wdpa = vect("C:/Users/jonas/Downloads/wdpa_410.gpkg")
wdpa = wdpa[which(wdpa$COUNTRY == "Gabon"), ]
wdpa = wdpa[, c("NAME_0", "NAME_1", "NAME_2", "NAME_3")]
ecobiome = vect("C:/data/storage/contextualization/ECOBIOME.gpkg",
                extent = ext(wdpa))
wdpa = vect("C:/data/storage/contextualization/WDPA.gpkg",
            extent = ext(wdpa))

wdpa2 = aggregate(wdpa)
wdpa = terra::intersect(wdpa, wdpa2)
nonwdpa = as.polygons(ext(wdpa)) - wdpa
nonwdpa$status = "not protected"
wdpa$status = "protected"
wdpa = rbind(wdpa, nonwdpa)
wdpa = aggregate(wdpa, by = "status")
wdpa = disagg(wdpa)

all = terra::intersect(wdpa, ecobiome)
all2 = terra::intersect(all, wdpa)

colras = rast("C:/data/Gabon_noXY.tiff")
colras[colras<50]=NA
colrasproj=project(colras,"epsg:3857")
writeRaster(colrasproj,"C:/data/gabon.tif")
vals = extract(colras > 50, all2, fun = "sum", na.rm = T)[, 2]
vals[is.na(vals)] = 0
highalertvals = extract(colras > 95, all2, fun = "sum", na.rm = T)[, 2]
highalertvals[is.na(highalertvals)] = 0
all2$events = vals
all2$highalerts = highalertvalsa
all2$agg_n=NULL
names(all2) = c(
  "country",
  "province",
  "district",
  "municipality",
  "ecoregion",
  "biome",
  "status",
  "events",
  "highalerts"
)
writeVector(all2, "C:/data/gabon_predictions_v1.shp", overwrite = T)
all2 = vect("C:/data/colombia_predictions_v2.gpkg")
#all2 = all2[-which(all2$events == 0), ]
all3 = simplifyGeom(all2, tolerance = 200 / 110000)

writeVector(all3, "C:/data/colombia_predictions_v3.gpkg", overwrite = T)
writeVector(all3, "C:/data/gabon_predictions_v2.shp", overwrite = T)
e <- erase(all3)
g <- gaps(e)
colras2 = colras
colras2[colras2 < 96] = NA

pols = as.polygons(colras2, dissolve = F)
polvals = extract(all2, centroids(pols))
polvals$events = NULL
polvals$highalerts = NULL
pols2 = cbind(pols, polvals)
writeVector(pols2, "C:/data/gabon_highalerts.shp")


###############dashboard version 3######
library(ForestForesight)
wdpa = vect("C:/Users/jonas/Downloads/wdpa_410.gpkg")
wdpa = wdpa[which(wdpa$COUNTRY == "Gabon"), ]
wdpa = wdpa[, c("NAME_0", "NAME_1", "NAME_2", "NAME_3")]
ecobiome = vect("C:/data/storage/contextualization/ECOBIOME.gpkg",
                extent = ext(wdpa))
wdpa = vect("C:/data/storage/contextualization/WDPA.gpkg",
            extent = ext(wdpa))

wdpa2 = aggregate(wdpa)
wdpa = terra::intersect(wdpa, wdpa2)
nonwdpa = as.polygons(ext(wdpa)) - wdpa
nonwdpa$status = "not protected"
wdpa$status = "protected"
wdpa = rbind(wdpa, nonwdpa)
wdpa = aggregate(wdpa, by = "status")
wdpa = disagg(wdpa)
wdpa$agg_n=NULL


colras = rast("C:/data/Gabon_noXY.tiff")
colras[colras<50]=NA
colrasproj=project(colras,"epsg:3857")
writeRaster(colrasproj,"C:/data/gabon.tif")
vals = extract(colras > 50, wdpa, fun = "sum", na.rm = T)[, 2]
vals[is.na(vals)] = 0
highalertvals = extract(colras > 95, all2, fun = "sum", na.rm = T)[, 2]
highalertvals[is.na(highalertvals)] = 0
wdpa$events = vals
wdpa$highalerts = highalertvals
writeVector(wdpa, "C:/data/gabon_predictions_wdpa.shp", overwrite = T)

ecobiome=terra::intersect(ecobiome,gadm2)
vals = extract(colras > 50, ecobiome, fun = "sum", na.rm = T)[, 2]
vals[is.na(vals)] = 0
highalertvals = extract(colras > 95, all2, fun = "sum", na.rm = T)[, 2]
highalertvals[is.na(highalertvals)] = 0
ecobiome$events = vals
ecobiome$highalerts = highalertvals
writeVector(ecobiome, "C:/data/gabon_predictions_ecobiome.shp", overwrite = T)

vals = extract(colras > 50, wdpa, fun = "sum", na.rm = T)[, 2]
vals[is.na(vals)] = 0
highalertvals = extract(colras > 95, all2, fun = "sum", na.rm = T)[, 2]
highalertvals[is.na(highalertvals)] = 0
wdpa$events = vals
wdpa$highalerts = highalertvals
writeVector(wdpa, "C:/data/gabon_predictions_wdpa.shp", overwrite = T)
names(all2) = c(
  "country",
  "province",
  "district",
  "municipality",
  "ecoregion",
  "biome",
  "status",
  "events",
  "highalerts"
)
writeVector(all2, "C:/data/gabon_predictions_v1.shp", overwrite = T)
all2 = vect("C:/data/colombia_predictions_v2.gpkg")
#all2 = all2[-which(all2$events == 0), ]
all3 = simplifyGeom(all2, tolerance = 200 / 110000)
all3$ecobiome=paste(all3$biome,"-",all3$ecoregion)

writeVector(all3, "C:/data/colombia_predictions_v3.gpkg", overwrite = T)
writeVector(all3, "C:/data/gabon_predictions_v2.shp", overwrite = T)
e <- erase(all3)
g <- gaps(e)
colras2 = colras
colras2[colras2 < 96] = NA

pols = as.polygons(colras2, dissolve = F)
polvals = extract(all2, centroids(pols))
polvals$events = NULL
polvals$highalerts = NULL
pols2 = cbind(pols, polvals)
writeVector(pols2, "C:/data/gabon_highalerts.shp")

