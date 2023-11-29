
source("C:/Users/EagleView/Documents/GitHub/ForestForesight/functions.R")
files=list.files("D:/ff-dev/alerts/",full.names=T)
files=sample(files,length(files))
ffdates=paste(sort(rep(c(2021,2022,2023),12)),seq(12),"01",sep="-")[1:29]
setwd("D:/ff-dev/results/")
for(file in files){
  print(file)
  for(ffdate in ffdates){
    print(ffdate)
    newfile=file.path(getwd(),createfilename(createrasname(file),date = ffdate,layer="layer"))
    diffdate=as.numeric(as.Date(ffdate)-as.Date("2015-01-01"))
    system(paste("python","C:/Users/EagleView/Documents/GitHub/ForestForesight/latestdate.py",file,newfile,diffdate),intern=T)
  }
}