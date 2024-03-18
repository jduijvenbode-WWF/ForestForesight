library(ForestForesight)
gadm=vect("C:/Users/jonas/Downloads/gadm_410.gpkg")
gadm=gadm[which(gadm$COUNTRY=="Gabon"),]
gadm=gadm[,c("NAME_0","NAME_1","NAME_2","NAME_3")]
ecobiome=vect("C:/data/storage/contextualization/ECOBIOME.gpkg",extent=ext(gadm))
wdpa=vect("C:/data/storage/contextualization/WDPA.gpkg",extent=ext(gadm))

gadm2=aggregate(gadm)
wdpa=terra::intersect(wdpa,gadm2)
nonwdpa=as.polygons(ext(wdpa))-wdpa
nonwdpa$status="not protected"
wdpa$status="protected"
wdpa=rbind(wdpa,nonwdpa)
wdpa=aggregate(wdpa,by="status")
wdpa=disagg(wdpa)
all=terra::intersect(gadm,ecobiome)
all2=terra::intersect(all,wdpa)

colras=rast("C:/data/colras3.tif")

vals=extract(colras>50,all2,fun="sum",na.rm=T)[,2]
vals[is.na(vals)]=0
highalertvals=extract(colras>95,all2,fun="sum",na.rm=T)[,2]
highalertvals[is.na(highalertvals)]=0
all2$events=vals
all2$highalerts=highalertvals
names(all2)=c("country","province","district","municipality","ecoregion","biome","status","events","highalerts")
writeVector(all2,"C:/data/colombia_predictions_v2.gpkg",overwrite=T)
all2=vect("C:/data/colombia_predictions_v2.gpkg")
all2=all2[-which(all2$events==0),]
all3=simplifyGeom(all2,tolerance=200/110000)

writeVector(all3,"C:/data/colombia_predictions_v3.gpkg",overwrite=T)
writeVector(all3,"C:/data/colombia_predictions_v3.shp",overwrite=T)
e <- erase(all3)
g <- gaps(e)
colras2=colras
colras2[colras2<96]=NA

pols=as.polygons(colras2,dissolve=F)
polvals=extract(all2,centroids(pols))
polvals$events=NULL
polvals$highalerts=NULL
pols2=cbind(pols,polvals)
writeVector(pols2,"C:/data/colombia_highalerts.gpkg")
