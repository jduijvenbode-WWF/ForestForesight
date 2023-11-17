files=list.files("D:/ff-dev/forestloss_2000_2022/",full.names=T)
firstindices=c("D:/ff-dev/forestloss_2000_2022/20N_080W.tif", "D:/ff-dev/forestloss_2000_2022/10N_080W.tif", "D:/ff-dev/forestloss_2000_2022/00N_080W.tif", "D:/ff-dev/forestloss_2000_2022/10N_070W.tif"
               ,"D:/ff-dev/forestloss_2000_2022/00N_070W.tif", "D:/ff-dev/forestloss_2000_2022/10S_080W.tif")
files=files[c(which(files %in% firstindices),which(!files %in% firstindices))]
maskboundaries=lapply(list.files("D:/ff-dev/forestmasks/",full.names = T),function(x) as.polygons(ext(rast(x))))
maskboundaries=do.call(rbind,maskboundaries)
maskboundaries$names=list.files("D:/ff-dev/forestmasks/",full.names = T)
for(file[2:length(files)] in files){
  loss=rast(file)
  mask=rast(maskboundaries[ext(loss)]$names,win=ext(loss))
  mask=mask-(loss<2020)
  maskb=boundaries(mask,filename="../edgedensitytemp.tif")
  
  loc=gsub(".tif","",(basename(file)))
  print(loc)
  template=rast(paste0("D:/ff-dev/results/",loc,"/elevation.tif"))
  project(mask,template,method="sum",filename=paste0("D:/ff-dev/results/",loc,"/forestmask2019.tif"))
  project(maskb,template,method="sum",filename=paste0("D:/ff-dev/results/",loc,"/edgedensity2019.tif"))
  
  
}
