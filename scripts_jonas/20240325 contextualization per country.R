library(ForestForesight)
data(countries)
setwd("C:/data")
proc_date <- "2024-04-01"
country <- "Peru"
countryiso <- "SUR"
threshold <- 0.95
dir.create(country)
setwd(country)
#shape <- terra::vect(countries)[which(countries$group==country),]
shape <- terra::vect(countries)[which(countries$iso3 == countryiso),]
b <- ForestForesight::train_predict_raster(shape = shape,prediction_date = proc_date,ff_folder = "C:/data/storage",verbose = T)
terra::writeRaster(b,paste0(country,"_",proc_date,".tif"),overwrite = T)
#b <- terra::rast(paste0(country,"_2024-01-01.tif"))

#########dashboard version 2########

gadm <- terra::vect("C:/Users/jonas/Downloads/gadm_410.gpkg")
gadm <- gadm[which(gadm$COUNTRY == country), ]

gadm <- gadm[, c("NAME_0", "NAME_1", "NAME_2", "NAME_3")]
gadm <- gadm[which(gadm$"NAME_1" == "Madre de Dios"),]
country <- "Madre de Dios"
ecobiome <- terra::vect("C:/data/storage/contextualization/ECOBIOME.gpkg",
                extent = terra::ext(gadm))
wdpa <- terra::vect("C:/data/storage/contextualization/WDPA.gpkg",
            extent = terra::ext(gadm))
ecobiome$ecobiome <- paste(ecobiome$Biome,ecobiome$Ecoregion,sep = "_")
wdpa2 <- aggregate(wdpa)
wdpa <- terra::intersect(wdpa, wdpa2)
nonwdpa <- terra::as.polygons(terra::ext(wdpa)) - wdpa
nonwdpa$status = "not protected"
wdpa$status = "protected"
wdpa <- rbind(wdpa, nonwdpa)
wdpa <- aggregate(wdpa, by = "status")
wdpa <- terra::disagg(wdpa)

all = terra::intersect(gadm, ecobiome)
all2 = terra::intersect(all, wdpa)

colras <- b
colras <- terra::mask(colras,gadm)
colras[colras < 0.50] <- NA
colras <- terra::crop(colras,gadm)
threshold <- as.numeric(terra::global(colras,fun = quantile,probs = 0.90,na.rm = T))
colrasproj <- terra::project(colras,"epsg:3857")
names(colrasproj) <- paste0("Forest Foresight predictions",country)
terra::writeRaster(colrasproj,paste0(country,".tif"),overwrite = T)
all2proj <- terra::project(all2,"epsg:3857")
vals <- terra::extract(colras > 0, all2proj, fun = "sum", na.rm = T)[, 2]
vals[is.na(vals)] <- 0
highalertvals <- terra::extract(colras > threshold, all2, fun = "sum", na.rm = T)[, 2]
highalertvals[is.na(highalertvals)] <- 0
all2$events <- vals
all2$highalerts <- highalertvals
all2$agg_n <- NULL
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
all2$proc_date <- proc_date
terra::writeVector(terra::centroids(all2), paste0(country,".overview.shp"), overwrite = T)

colras2 = colras
colras2[colras2 < threshold] = NA

pols = terra::as.polygons(terra::project(colras2, terra::crs(all2)), dissolve = F)
polvals = terra::extract(all2, terra::centroids(pols))
polvals$events = NULL
polvals$highalerts = NULL
pols2 = cbind(pols, polvals)
pols2$x = as.numeric(round(terra::crds(terra::centroids(pols2))[,1],3))
pols2$y = as.numeric(round(terra::crds(terra::centroids(pols2))[,2],3))
terra::writeVector(terra::project(pols2,"epsg:3857"), paste0(country,"_highalerts.shp"),overwrite = T)
#code to extract the high alert values and amount of normal alerts for wdpa, ecobiome and gadm separately
wdpa2 = terra::intersect(wdpa, aggregate(gadm))
eventcount = terra::project(colras > 0,terra::crs(ecobiome))
wdpa2$events <- terra::extract(eventcount,wdpa2,fun = "sum",na.rm = T)[,2]
wdpa2$events[which(is.na(wdpa2$events))] = 0
wdpa2$x = as.numeric(round(terra::crds(terra::centroids(wdpa2))[,1],3))
wdpa2$y = as.numeric(round(terra::crds(terra::centroids(wdpa2))[,2],3))
terra::writeVector(terra::project(wdpa2,"epsg:3857"), paste0(country,"_wdpa.shp"),overwrite = T)

ecobiome2 <- terra::intersect(ecobiome, aggregate(gadm))
ecobiome2$events <- terra::extract(eventcount,ecobiome2,fun = "sum",na.rm = T)[,2]
ecobiome2$events[which(is.na(ecobiome2$events))] <- 0
ecobiome2$x <- as.numeric(round(terra::crds(terra::centroids(ecobiome2))[,1],3))
ecobiome2$y <- as.numeric(round(terra::crds(terra::centroids(ecobiome2))[,2],3))
terra::writeVector(terra::project(ecobiome2,"epsg:3857"), paste0(country,"_ecobiome.shp"),overwrite = T)
names(gadm) <- c("country","province","district","municipality")

gadm$events <- terra::extract(eventcount,gadm,fun = "sum",na.rm = T)[,2]
gadm$events[which(is.na(gadm$events))] <- 0
gadm$highevents <- round(terra::extract(terra::project(colras2,gadm),gadm,fun = "sum",na.rm = T)[,2])
gadm$highevents[which(is.na(gadm$highevents))] <- 0
gadm$label <- paste0(gadm$district,"\n",gadm$municipality)
head(gadm)
gadm$x = as.numeric(round(terra::crds(terra::centroids(gadm))[,1],3))
gadm$y = as.numeric(round(terra::crds(terra::centroids(gadm))[,2],3))
terra::writeVector(terra::project(gadm,"epsg:3857"), paste0(country,"_gadm.shp"),overwrite = T)
files <- list.files()
files <- files[unique(c(grep("dbf$",files),grep("prj$",files),grep("shp$",files),grep("shx$",files),grep("cpg$",files)))]
if (file.exists(paste0(country,"_contextualization.zip"))) {file.remove(paste0(country,"_contextualization.zip"))}
zip(paste0(country,"_contextualization.zip"),files)
