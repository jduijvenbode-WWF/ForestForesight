library(lubridate)
library(ForestForesight)
library(parallel)
maxdate=commandArgs(trailingOnly = T)
#maxdate="2024-02-01"
if(length(maxdate)==0){stop("no date given till when to process")}
gtdate1m <- as.character(ymd(maxdate) %m-% months(1))
gtdate3m <- as.character(ymd(maxdate) %m-% months(3))
gtdate6m <- as.character(ymd(maxdate) %m-% months(6))
gtdate12m <- as.character(ymd(maxdate) %m-% months(12))



ffdates=paste(sort(rep(seq(2021,2030),12)),sprintf("%02d",seq(12)),"01",sep="-")[]
ffdates=ffdates[which(ymd(ffdates)<=ymd(maxdate))]
layers=c("timesinceloss","patchdensity","lastsixmonths","totallossalerts","previoussameseason","confidence","smoothedtotal","smoothedsixmonths","lastmonth","groundtruth1m","groundtruth3m","groundtruth6m","groundtruth12m")
comb1=apply(expand.grid(ffdates, layers), 1, paste, collapse="_")

data("gfw_tiles")
tiles=vect(gfw_tiles)$tile_id
comb2=apply(expand.grid(tiles, comb1), 1, paste, collapse="_")
allfiles=paste0(file.path("D:/ff-dev/results/preprocessed/input",substr(comb2,1,8),comb2),".tif")
allfiles=c(allfiles[-grep("groundtruth",allfiles)],gsub("input","groundtruth",allfiles[grep("groundtruth",allfiles)]))
allfiles=allfiles[-which(grepl("groundtruth1m",basename(allfiles))&(ymd(gtdate1m)<ymd(substr(basename(allfiles),10,21))))]
allfiles=allfiles[-which(grepl("groundtruth3m",basename(allfiles))&(ymd(gtdate3m)<ymd(substr(basename(allfiles),10,21))))]
allfiles=allfiles[-which(grepl("groundtruth6m",basename(allfiles))&(ymd(gtdate6m)<ymd(substr(basename(allfiles),10,21))))]
allfiles=allfiles[-which(grepl("groundtruth12m",basename(allfiles))&(ymd(gtdate12m)<ymd(substr(basename(allfiles),10,22))))]
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
                  "--groundtruth1m",
                  as.numeric(!(as.Date(substr(basename(utbp),10,19))>as.Date(gtdate1m))),
                  "--groundtruth3m",
                  as.numeric(!(as.Date(substr(basename(utbp),10,19))>as.Date(gtdate3m))),
                  "--groundtruth6m",
                  as.numeric(!(as.Date(substr(basename(utbp),10,19))>as.Date(gtdate6m))),
                  "--groundtruth12m",
                  as.numeric(!(as.Date(substr(basename(utbp),10,19))>as.Date(gtdate12m))))

cl <- makeCluster(getOption("cl.cores", 9))
clusterExport(cl, "commandtxts")
results <- clusterApply(cl, commandtxts, system)

# Stop the cluster
stopCluster(cl)

