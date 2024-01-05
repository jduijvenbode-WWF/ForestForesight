startras="C:/Users/jonas/Downloads/10N_80W_forestmask_unprojected4.tif"
templateras="C:/Users/jonas/Downloads/10N_080W/elevation.tif"
r <- rast(startras)
  selpols=as.polygons(rast(ext(rast(r)),ncol=4,nrow=4))
  raslist=list()
  for(i in 1:length(selpols)){
    buffsize=res(rast(startras))[1]*110000
    selras=rast(startras,win=buffer(selpols[i],buffsize))
    writeRaster(selras,"C:/data/tempras.tif",overwrite=T)
    system(paste0("python C:/data/xgboost_test/sobelfilter.py C:/data/tempras.tif C:/data/edges",i,".tif"),intern=T)
    rasts=list.files("C:/data/",full.names = T,pattern="edges")
    newrast=project(merge(sprc(rasts)),rast(templateras),method="sum",filename="C:/data/forestmask_new.tif")
    print(i)
  }

