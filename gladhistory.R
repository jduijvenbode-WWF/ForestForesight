files=list.files("D:/ff-dev/forestloss_2000_2022/",full.names=T)

firstindices=c("D:/ff-dev/forestloss_2000_2022/20N_080W.tif", "D:/ff-dev/forestloss_2000_2022/10N_080W.tif", "D:/ff-dev/forestloss_2000_2022/00N_080W.tif", "D:/ff-dev/forestloss_2000_2022/10N_070W.tif"
               ,"D:/ff-dev/forestloss_2000_2022/00N_070W.tif", "D:/ff-dev/forestloss_2000_2022/10S_080W.tif")
files=files[c(which(files %in% firstindices),which(!files %in% firstindices))]

for(file in files){
  loss=rast(file)
  loc=gsub(".tif","",(basename(file)))
  print(loc)
  if(file.exists(paste0("D:/ff-dev/results/",loc,"/elevation.tif"))){
    if(nrow(rast(paste0("D:/ff-dev/results/",loc,"/lastloss.tif")))==1250){
      template=rast(paste0("D:/ff-dev/results/",loc,"/elevation.tif"))
      project(loss==22,template,method="sum",filename=paste0("D:/ff-dev/results/",loc,"/loss2022.tif"),overwrite=T)
      project(loss==21,template,method="sum",filename=paste0("D:/ff-dev/results/",loc,"/loss2021.tif"),overwrite=T)
      project(loss==20,template,method="sum",filename=paste0("D:/ff-dev/results/",loc,"/loss2020.tif"),overwrite=T)
      project(loss==19,template,method="sum",filename=paste0("D:/ff-dev/results/",loc,"/loss2019.tif"),overwrite=T)
      project((loss>0)*(loss<19),template,method="sum",filename=paste0("D:/ff-dev/results/",loc,"/loss2001-2018.tif"),overwrite=T)
      project(loss,template,method="max",filename=paste0("D:/ff-dev/results/",loc,"/lastloss.tif"),overwrite=T)
    }
  }
  
}
