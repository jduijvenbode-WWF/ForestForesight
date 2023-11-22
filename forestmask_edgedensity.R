library(terra)
files=list.files("C:/users/jonas/downloads/forestloss_2000_2022/",full.names=T)
firstindices=c("C:/users/jonas/downloads/forestloss_2000_2022/20N_080W.tif", "C:/users/jonas/downloads/forestloss_2000_2022/10N_080W.tif", "C:/users/jonas/downloads/forestloss_2000_2022/00N_080W.tif", "C:/users/jonas/downloads/forestloss_2000_2022/10N_070W.tif"
               ,"C:/users/jonas/downloads/forestloss_2000_2022/00N_070W.tif", "C:/users/jonas/downloads/forestloss_2000_2022/10S_080W.tif")
files=files[c(which(files %in% firstindices),which(!files %in% firstindices))]

maskboundaries=lapply(list.files("C:/users/jonas/downloads/forestmasks/",full.names = T),function(x) as.polygons(ext(rast(x))))
maskboundaries=do.call(rbind,maskboundaries)
maskboundaries$names=list.files("C:/users/jonas/downloads/forestmasks/",full.names = T)
m=c(1,19,1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
for(file in files){
  loc=gsub(".tif","",(basename(file)))
  print(loc)
  edgesfile=paste0("C:/users/jonas/downloads/elevations/",loc,"_edgedensity2019.tif")
  if(!file.exists(edgesfile)){
    elefile=paste0("C:/users/jonas/downloads/elevations/",loc,"_elevation.tif")
    if(file.exists(elefile)){
      loss=rast(file)
      masknames=maskboundaries[ext(loss)]$names
      if(length(masknames)>0){
        if(length(masknames)>1){
          mask=crop(merge(sprc(masknames)),ext(loss))
        }else{mask=rast(masknames,win=ext(loss))}
        if(ext(mask)!=ext(loss)){mask=extend(mask,loss,fill=0)}
        mask=mask-classify(loss,rclmat,others=0)
        template=rast(paste0("C:/users/jonas/downloads/elevations/",loc,"_elevation.tif"))
        forestmask=paste0("C:/users/jonas/downloads/elevations/",loc,"_forestmask2019.tif")
        if(!file.exists(forestmask)){project(mask,template,method="sum",filename=forestmask)}
        selpols=as.polygons(rast(ext(mask),ncol=2,nrow=2))
        for(i in 1:length(selpols)){
          buffsize=res(mask)[1]*110000
          crop(mask,buffer(selpols[i],buffsize),filename="C:/data/tempras.tif",overwrite=T)
          system(paste0("python C:/data/git/ForestForesight/sobelfilter.py C:/data/tempras.tif C:/data/edges",i,".tif"),intern=T)
        }
        rasts=list.files("C:/data/",full.names = T,pattern="edges")
        newrast=project(merge(sprc(rasts)),template,method="sum",filename=edgesfile)
        file.remove(rasts)
      }
    }
  }
}
