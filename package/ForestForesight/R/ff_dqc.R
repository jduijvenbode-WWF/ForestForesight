#' Data quality control
#'
#' This function loops through all tif files in a given folder and gives a summary and total overview of the quality of the rasters
#'
#' @param folder_path The path to the folder containing TIF files.
#'
#' @examples
#' # Example usage:
#' result <- process_tif_files("path/to/tif_folder")
#'
#' @export
#'
#'
ff_dqc <- function(folder_path) {
  summary_by_feature=function(dataframe,feature){
    type="dynamic"
    seldf=dataframe[which(dataframe$featurenames==feature),]
    if(nrow(seldf)==1){seldf=rbind(seldf,seldf);type="static"}
    return(list("feature"=feature,
                "type"=type,
                "mindate"=if(is.na(seldf$dates[1])){NA}else{as.character(min(as.Date(seldf$dates)))},
                "maxdate"=if(is.na(seldf$dates[1])){NA}else{as.character(max(as.Date(seldf$dates)))},
                "gaps"=if(max(diff(sort(as.Date(seldf$dates))))<32){"no"}else{"yes"},
                "doubles"=if(min(diff(sort(as.Date(seldf$dates))))>27){"no"}else{"yes"},
                "npixel"=if(sd(seldf$npixel)==0){seldf$npixel[1]}else{"diff"},
                "xmin"=if(sd(seldf$xmin)==0){seldf$xmin[1]}else{"diff"},
                "xmax"=if(sd(seldf$xmax)==0){seldf$xmax[1]}else{"diff"},
                "ymin"=if(sd(seldf$ymin)==0){seldf$ymin[1]}else{"diff"},
                "ymax"=if(sd(seldf$ymax)==0){seldf$ymax[1]}else{"diff"},
                "resolution"=if(sd(seldf$resolution)==0){seldf$resolution[1]}else{"diff"},
                "crsname"=if(length(unique(seldf$crsname))==1){seldf$crsname[1]}else{"diff"},
                "crscode"=if(length(unique(seldf$crscode))==1){seldf$crscode[1]}else{"diff"},
                "mean"=round(mean(as.numeric(seldf$mean)),2),
                "max"=round(max(as.numeric(seldf$max)),2),
                "hasNA"=if(length(unique(seldf$hasNA))==1){seldf$hasNA[1]}else{"diff"}))
  }
  # Get a list of all TIF files in the folder
  tif_files <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)
  valuelist=lapply(tif_files,function(x) ff_dqc_file(x))
  allvals=as.data.frame(as.matrix(do.call(rbind,valuelist)))
  names(allvals)=names(valuelist[[1]])
  for(i in 1:ncol(allvals)){allvals[,i]=unlist(allvals[,i])}
  allvals$featurenames=as.character(unlist(sapply(tif_files,function(x) strsplit(gsub(pattern = "\\.tif",replacement="",basename(x)),"_")[[1]][1])))
  allvals$dates=as.character(unlist(sapply(tif_files,function(x) strsplit(gsub(pattern = "\\.tif",replacement="",basename(x)),"_")[[1]][2])))
  incorrect_dateformats=sum(!(nchar(allvals$dates)==10))
  allvals=allvals[,c(ncol(allvals)-1,ncol(allvals),seq(ncol(allvals)-2))]
  # Initialize data frame to store quality flags
  summary=suppressWarnings(lapply(unique(allvals$featurenames),function(x) summary_by_feature(allvals,x)))
  summarytable=as.data.frame(do.call(rbind,summary))
  names(summarytable)=names(summary[[1]])
  for(i in 1:ncol(summarytable)){summarytable[,i]=unlist(summarytable[,i])}
  summarytable=summarytable[order(summarytable$type),]
  dyn_summary=summarytable[which(!is.na(summarytable$mindate)),]
  datecheck=all(dyn_summary$gaps=="no")&all(dyn_summary$doubles=="no")&(length(unique(dyn_summary$mindate))==1)&(length(unique(dyn_summary$maxdate))==1)
  extentcheck=length(c(unique(allvals$npixel),unique(allvals$xmin),unique(allvals$xmax),unique(allvals$ymin),unique(allvals$ymax),unique(allvals$resolution)))==6
  return(list("tile"=basename(folder_path),"byfeature"=summarytable,"all"=allvals,"equalextent"=extentcheck,"equaldaterange"=datecheck,"incorrect_dateformats"=incorrect_dateformats))
}
