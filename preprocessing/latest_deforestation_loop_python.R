
files=list.files("D:/ff-dev/alerts/",full.names=T)
files=sample(files,length(files))
ffdates=paste(sort(rep(c(2021,2022,2023),12)),sprintf("%02d",seq(12)),"01",sep="-")[1:36]
setwd("D:/ff-dev/results/preprocessed/")
for(file in files){
  for(ffdate in ffdates){
    print(paste(file,ffdate))
    newfile=file.path(getwd(),substr(basename(file),1,8),paste0(substr(basename(file),1,8),"_",ffdate,"_layer.tif"))
    diffdate=as.numeric(as.Date(ffdate)-as.Date("2015-01-01"))
    commandtxt=paste("python","C:/Users/EagleView/Documents/GitHub/ForestForesight/preprocessing/IA-processing.py",file,newfile,diffdate)
    if(ffdate>"2023-06-01"){
      commandtxt=paste("python","C:/Users/EagleView/Documents/GitHub/ForestForesight/preprocessing/IA-processing.py",file,newfile,diffdate,"--groundtruth 0")
    }
    system(commandtxt)
    if(file.exists("D:/ff-dev/stop.txt")){stop()}
  }
}
