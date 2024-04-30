tempras=list.files("C:/data/storage/predictions/",pattern="2024-01-01_predictions.tif",full.names = T,recursive=T)
GADMv=as.data.frame(vect("C:/data/storage/contextualization/GADM.gpkg"))
GADMv$ID=seq(nrow(GADMv))
EBv=as.data.frame(vect("C:/data/storage/contextualization/ECOBIOME.gpkg"))
EBv$ID=seq(nrow(EBv))
for(predras in tempras[c(10,47,48)]){
  print(predras)
  GADM=file.path("C:/data/storage/contextualization/GADM",paste0(substr(basename(predras),1,8),"_2021-01-01_GADM.tif"))
  WDPA=file.path("C:/data/storage/contextualization/WDPA",paste0(substr(basename(predras),1,8),"_2021-01-01_WDPA.tif"))
  ECOBIOME=file.path("C:/data/storage/contextualization/ECOBIOME",paste0(substr(basename(predras),1,8),"_2021-01-01_ECOBIOME.tif"))
  preds=rast(predras)
  GADMp=rast(GADM,win=ext(preds))
  WDPAp=rast(WDPA,win=ext(preds))
  ECOBIOMEp=rast(ECOBIOME,win=ext(preds))

  predmat=as.matrix(c(preds,WDPAp,GADMp,ECOBIOMEp))
  colnames(predmat)=c("certainty","WDPA","GADM","ECOBIOME")
  indices=which(predmat[,1]>50)
  if(length(indices)>1){
    predmat=predmat[indices,]
    classras=preds>50
    classras=classras/classras
    pols=as.polygons(classras,dissolve=F,na.rm=T)
    pols$ID=seq(length(pols))
    predmat=as.data.frame(predmat)
    predmat$ID=seq(nrow(predmat))
    predmat=merge(predmat,GADMv,by.x="GADM",by.y="ID",all.x=T)
    predmat=merge(predmat,EBv,by.x="ECOBIOME",by.y="ID",all.x=T)
    predmat$WDPA=unlist(sapply(predmat$WDPA,function(x) if(x==1){"ïnside WDPA"}else{"outside WDPA"}))
    predmat$testtiff=NULL
    predmat$GADM=NULL
    predmat$ECOBIOME=NULL
    predmat[is.na(predmat)]="Unknown"
    pols2=terra::merge(pols,predmat,by="ID")
    pols2$testtiff=NULL
    if(!exists("allpols")){allpols=pols2}else{allpols=rbind(allpols,pols2)}
    # dir.create(file.path("C:/data/storage/hotzones/",substr(basename(predras),1,8)))
    # writeVector(pols2,file.path("C:/data/storage/hotzones/",substr(basename(predras),1,8),paste0(substr(basename(predras),1,8),"_hotzones.json")),overwrite=T)
    # writeVector(centroids(pols2),file.path("C:/data/storage/hotzones/",substr(basename(predras),1,8),paste0(substr(basename(predras),1,8),"_hotzones_centers.json")),overwrite=T)
  }
}

setwd("C:/data/storage/contextualization/ECOBIOME/")
files=list.files()
for(file in files){file.rename(file,gsub("GADM","ECOBIOME",file))}
test=c(rast(nrows=18,ncols=36,vals=seq(18*36)),rast(nrows=18,ncols=36,vals=seq(18*36)+10))
