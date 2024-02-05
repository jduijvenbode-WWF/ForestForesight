datafolder='D:/ff-dev/nighttime/converted2/'
downloadfolder="D:/ff-dev/nighttime/VNP46A3/2023/335/"
setwd(downloadfolder)
files=list.files(path=downloadfolder,recursive=T,pattern="h5$",full.names=T)
print(length(files))

library(ForestForesight)
data("gfw_tiles")
areas=vect(gfw_tiles)
for(file in files){
  curdate=as.Date(as.numeric(basename(dirname(file))),origin=paste0(as.numeric(basename(dirname(dirname(file))))-1,"-12-31"))
    col=as.numeric(substr(basename(file),19,20))
    row=as.numeric(substr(basename(file),22,23))
    coln=if(col<18){sprintf("%02d0W",18-col)}else{sprintf("%02d0E",col-18)}
    rown=if(row<10){sprintf("%01d0N",9-row)}else{sprintf("%01d0S",row-9)}
    colo=10*(col-18)
    rowo=10*(9-row)
    foldername=paste0(rown,"_",coln)
    if(foldername %in% areas$tile_id){
      extractname=paste0(datafolder,foldername,"/",foldername,"_",curdate,"_nightlights.tif")
      if(!file.exists(extractname)){
        if(!dir.exists(file.path(datafolder,foldername))){dir.create(file.path(datafolder,foldername))}
        cat(foldername);cat(curdate);cat("\n")
        res=system(paste("gdal_translate -a_srs EPSG:4326 -a_nodata 65535 -a_ullr",colo,rowo,colo+10,rowo-10,
                     paste0('-of GTiff HDF5:"',file,'"://HDFEOS/GRIDS/VIIRS_Grid_DNB_2d/Data_Fields/NearNadir_Composite_Snow_Free ',
                            extractname)),intern=T)
        print(res)
        tempras=list.files(file.path("D:/ff-dev/results/preprocessed/",basename(dirname(extractname))),pattern="elev",full.names = T)[1]
        newname=file.path(dirname(tempras),basename(extractname))
        print(extractname)
        print(tempras)
        print(newname)
        print(file.exists(extractname))
        project(rast(extractname),rast(tempras),method="near",filename=newname,overwrite=T)
        #file.remove(extractname)
      }
    }
  }

