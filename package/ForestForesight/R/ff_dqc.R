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
    seldf=dataframe[which(dataframe$featurenames==feature),]
    if(nrow(seldf)==1){seldf=rbind(seldf,seldf)}
    return(list("feature"=feature,
                "mindate"=as.character(min(as.Date(seldf$dates))),
                "maxdate"=as.character(max(as.Date(seldf$dates))),
                "gaps"=if(max(diff(sort(as.Date(seldf$dates))))<32){"no"}else{"yes"},
                "doubles"=if(min(diff(sort(as.Date(seldf$dates))))>27){"no"}else{"yes"},
                "npixel"=if(sd(seldf$npixel)==0){"equal"}else{"diff"},
                "xmin"=if(sd(seldf$xmin)==0){"equal"}else{"diff"},
                "xmax"=if(sd(seldf$xmax)==0){"equal"}else{"diff"},
                "ymin"=if(sd(seldf$ymin)==0){"equal"}else{"diff"},
                "ymax"=if(sd(seldf$ymax)==0){"equal"}else{"diff"},
                "resolution"=if(sd(seldf$resolution)==0){"equal"}else{"diff"},
                "crsname"=if(length(unique(seldf$crsname))==1){"equal"}else{"diff"},
                "crscode"=if(length(unique(seldf$crscode))==1){"equal"}else{"diff"},
                "mean"=round(mean(as.numeric(seldf$mean)),2),
                "max"=round(max(as.numeric(seldf$max)),2),
                "hasNA"=if(length(unique(seldf$hasNA))==1){"equal"}else{"diff"}))
  }
  # Get a list of all TIF files in the folder
  tif_files <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)
  valuelist=lapply(tif_files,function(x) summarize_rast(x))
  allvals=as.data.frame(as.matrix(do.call(rbind,valuelist)))
  names(allvals)=names(valuelist[[1]])
  for(i in 1:ncol(allvals)){allvals[,i]=unlist(allvals[,i])}
  allvals$featurenames=as.character(unlist(sapply(tif_files,function(x) strsplit(gsub(pattern = "\\.tif",replacement="",basename(x)),"_")[[1]][1])))
  allvals$dates=as.character(unlist(sapply(tif_files,function(x) strsplit(gsub(pattern = "\\.tif",replacement="",basename(x)),"_")[[1]][2])))
  allvals=allvals[,c(ncol(allvals)-1,ncol(allvals),seq(ncol(allvals)-2))]
  # Initialize data frame to store quality flags
  summary=lapply(unique(allvals$featurenames),function(x) summary_by_feature(allvals,x))
  summarytable=as.data.frame(do.call(rbind,summary))
  names(summarytable)=names(summary[[1]])
  for(i in 1:ncol(summarytable)){summarytable[,i]=unlist(summarytable[,i])}
  return(list("summary"=summarytable,"all"=allvals))
}
