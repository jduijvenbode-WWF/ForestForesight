
# Load the argparse package
library(argparse)

# Create argument parser
parser <- ArgumentParser(description = "Script for processing data with specified options")

# Add arguments
parser$add_argument("-d", "--max-date",
                    dest = "max_date",
                    default = format(Sys.Date(), "%Y-%m-01"),
                    help = "Maximum date (default: first of the current month)")

parser$add_argument("-c", "--cores",
                    dest = "cores",
                    default = "9",
                    help = "Number of cores (default: 9)")

parser$add_argument("-s", "--script-location",
                    dest = "script_location",
                    required = FALSE,
                    default= "C:/Users/EagleView/Documents/GitHub/ForestForesight/preprocessing/IA-processing.py",
                    help = "Location of the processing script (must be a Python script)")
parser$add_argument("-i", "--input_folder",
                    dest = "script_location",
                    required = FALSE,
                    default= "D:/ff-dev/alerts/",
                    help = "Location of the input folder that contains the GFW integrated alert tif files")
parser$add_argument("-p", "--prep_folder",
                    dest = "prep_folder",
                    required = FALSE,
                    default= "D:/ff-dev/results/preprocessed/",
                    help = "Location of the preprocessed data folder")

# Parse arguments
args <- parser$parse_args()
if(!dir.exists(args$input_folder)){stop("input folder does not exist")}
if(!dir.exists(args$prep_folder)){stop("preprocessed data folder does not exist")}
# Check if the script location ends with ".py"
if (!grepl("\\.py$", args$script_location)) {
  stop("Error: The script location must be a Python script (*.py)")
}
if(!file.exists(args$script_location)){stop("the given python script does not exist")}
# Print parsed arguments
cat("Maximum date:", args$max_date, "\n")
cat("Number of cores:", args$cores, "\n")
cat("Script location:", args$script_location, "\n")
cores=as.numeric(args$cores)
if(is.na(cores)){stop("core count was not a number")}

library(ForestForesight)
library(parallel)

#maxdate="2024-02-01"
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
allfiles=paste0(file.path(args$prep_folder,"input",substr(comb2,1,8),comb2),".tif")
allfiles=c(allfiles[-grep("groundtruth",allfiles)],gsub("input","groundtruth",allfiles[grep("groundtruth",allfiles)]))
allfiles=allfiles[-which(grepl("groundtruth1m",basename(allfiles))&(ymd(gtdate1m)<ymd(substr(basename(allfiles),10,21))))]
allfiles=allfiles[-which(grepl("groundtruth3m",basename(allfiles))&(ymd(gtdate3m)<ymd(substr(basename(allfiles),10,21))))]
allfiles=allfiles[-which(grepl("groundtruth6m",basename(allfiles))&(ymd(gtdate6m)<ymd(substr(basename(allfiles),10,21))))]
allfiles=allfiles[-which(grepl("groundtruth12m",basename(allfiles))&(ymd(gtdate12m)<ymd(substr(basename(allfiles),10,22))))]
tobeprocessed=allfiles[which(!file.exists(allfiles))]
tobeprocessed=gsub("/groundtruth/","/input/",tobeprocessed)
utbp=unique(file.path(dirname(tobeprocessed),paste0(substr(basename(tobeprocessed),1,19),"_layer.tif")))
utbp=utbp[grep("layer",utbp)]
utbp=unique(utbp)
utbp=sample(utbp,length(utbp))
commandtxts=paste("python",
                  args$script_location,
                  paste0(args$input_folder,basename(dirname(utbp)),".tif"),
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

cl <- makeCluster(getOption("cl.cores", cores))
clusterExport(cl, "commandtxts")
results <- clusterApply(cl, commandtxts, system)

# Stop the cluster
stopCluster(cl)

