latestdeforestation=function(ras,diffdate,filename){
  if(!file.exists(filename)){
    aggregate(diffdate-(ras%%10000),40,fun="max",na.rm=T,filename=filename,overwrite=T)
  }
}
files=list.files("D:/ff-dev/alerts/",full.names=T)
firstindices=c("D:/ff-dev/alerts/20N_080W.tif", "D:/ff-dev/alerts/10N_080W.tif", "D:/ff-dev/alerts/00N_080W.tif", "D:/ff-dev/alerts/10N_070W.tif"
               ,"D:/ff-dev/alerts/00N_070W.tif", "D:/ff-dev/alerts/10S_080W.tif")
files=files[c(which(files %in% firstindices),which(!files %in% firstindices))]
firstindices=c("D:/ff-dev/alerts/10N_080W.tif")
files=files[c(which(files %in% firstindices),which(!files %in% firstindices))]

ffdates=paste(sort(rep(c(2021,2022,2023),12)),seq(12),"01",sep="-")
for(file in files){
  ras=rast(file)
  for(ffdate in ffdates){
    print(ffdate)
    diffdate=as.numeric(as.Date(ffdate)-as.Date("2015-01-01"))
    rasname=createrasname(file)
    newfilename=createfilename(rasname,date=ffdate,layer="latestdeforestation",number=NA)
    if(!file.exists(newfilename)){
      latestdeforestation(ras,diffdate,newfilename)
    }
  }
}
  