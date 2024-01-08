
source("C:/Users/EagleView/Documents/GitHub/ForestForesight/functions.R")
files=list.files("D:/ff-dev/alerts/",full.names=T)
files=sample(files,length(files))
ffdates=paste(sort(rep(c(2021,2022,2023),12)),sprintf("%02d",seq(12)),"01",sep="-")[1:29]
setwd("D:/ff-dev/results/")
for(file in files){
  print(file);print(Sys.time())
  for(ffdate in ffdates){
    print(ffdate)
    newfile=file.path(getwd(),createfilename(createrasname(file),date = ffdate,layer="layer"))
    diffdate=as.numeric(as.Date(ffdate)-as.Date("2015-01-01"))
    commandtxt=paste("python","C:/Users/EagleView/Documents/GitHub/ForestForesight/IA-processing.py",file,newfile,diffdate)
    print(commandtxt)
    system(commandtxt,intern=T)
  }
}
