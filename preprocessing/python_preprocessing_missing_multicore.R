library(lubridate)
library(ForestForesight)
library(parallel)
maxdate=commandArgs(trailingOnly = T)
#maxdate="2024-02-01"
if(length(maxdate)==0){stop("no date given till when to process")}
gtdate <- as.character(ymd(maxdate) %m-% months(6))
print(gtdate)


ffdates=paste(sort(rep(seq(2021,2030),12)),sprintf("%02d",seq(12)),"01",sep="-")[]
ffdates=ffdates[which(ymd(ffdates)<=ymd(maxdate))]
layers=c("timesinceloss","patchdensity","lastsixmonths","totallossalerts","previoussameseason","confidence","smoothedtotal","smoothedsixmonths","groundtruth6mbin","lastmonth","groundtruth1m","groundtruth3m","groundtruth6m","groundtruth12m")
comb1=apply(expand.grid(ffdates, layers), 1, paste, collapse="_")

data("gfw_tiles")
tiles=vect(gfw_tiles)$tile_id
comb2=apply(expand.grid(tiles, comb1), 1, paste, collapse="_")
allfiles=paste0(file.path("D:/ff-dev/results/preprocessed/input",substr(comb2,1,8),comb2),".tif")
allfiles=c(allfiles[-grep("groundtruth",allfiles)],gsub("input","groundtruth",allfiles[grep("groundtruth",allfiles)]))
allfiles=allfiles[-which(grepl("groundtruth",basename(allfiles))&(ymd(gtdate)<ymd(substr(basename(allfiles),10,19))))]
tobeprocessed=allfiles[which(!file.exists(allfiles))]
utbp=unique(file.path(dirname(tobeprocessed),paste0(substr(basename(tobeprocessed),1,19),"_layer.tif")))
utbp=utbp[grep("layer",utbp)]
utbp=unique(utbp)
utbp=sample(utbp,length(utbp))
commandtxts=paste("python",
                  "C:/Users/EagleView/Documents/GitHub/ForestForesight/preprocessing/IA-processing.py",
                  paste0("D:/ff-dev/alerts/",basename(dirname(utbp)),".tif"),
                  utbp,
                  as.numeric(as.Date(substr(basename(utbp),10,19))-as.Date("2015-01-01")),
                  "--groundtruth",
                  as.numeric(!(as.Date(substr(basename(utbp),10,19))>as.Date(gtdate))))

cl <- makeCluster(getOption("cl.cores", 9))
clusterExport(cl, "commandtxts")
results <- clusterApply(cl, commandtxts, system)

# Stop the cluster
stopCluster(cl)

