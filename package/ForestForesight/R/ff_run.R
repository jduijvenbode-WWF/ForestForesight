#' Run ForestForesight prediction on specified tiles
#'
#' @param datafolder Path to the folder containing the input data.
#' @param tiles Vector of tile names to process (e.g. 10N_080W).
#' @param model Trained model object. If NULL, a new model will be trained.
#' @param start Start date for training data (default: c(2021, 1)).
#' @param end End date for training data (default: c(2021, 12)).
#' @param pstart Start date for prediction (default: c(2022, 5)).
#' @param pend End date for prediction (default: c(2023, 5)).
#' @param savemodel Logical indicating whether to save the trained model (default: TRUE).
#' @param savefolder Path to the folder where models and predictions will be saved.
#' @param analyse Logical indicating whether to perform analysis on interest polygons (default: TRUE).
#' @param overwrite Logical indicating whether to overwrite existing prediction files (default: FALSE).
#' @param csvfile Path to the CSV file for storing analysis results.
#' @param append_to_csv Logical indicating whether to append results to the existing CSV file (default: TRUE for append, else overwrite).
#'
#' @return None
#' @export
#'
#'

ff_run=function(datafolder,tiles,model=NULL,start=c(2021,1),end=c(2021,12),pstart=c(2022,5),pend=c(2023,5),savemodel=T,savefolder=NULL,analyse=T,overwrite=F,csvfile=NULL,append_to_csv=T){
  if(is.null(model)){
    window=ff_dqc(file.path(datafolder,tile))$minextent
    if(!dir.exists(file.path(savefolder,tile))){dir.create(file.path(savefolder,tile))}
    prepped_data=ForestForesight::ff_prep(datafolder,tiles = tile,start=start,end=end,shrink = "extract",window = window)
    model=ff_train(train_matrix = prepped_data$data_matrix)
  }
  if(savemodel){saveRDS(model,file.path(savefolder,tile,"predictor.rds"))}
  daterange=as.character(seq(as.Date(paste0(pstart[1],"-",sprintf("%02d",pstart[2]),"-01")),as.Date(paste0(pend[1],"-",sprintf("%02d",pend[2]),"-01")),"1 month"))
  for(i in daterange){
    newfilename=file.path(savefolder,tile,paste0("predictions_",i,".tif"))
    if(overwrite|!file.exists(newfilename)){
      future=ForestForesight::ff_prep(datafolder,tiles = tile,start=c(lubridate::year(i),lubridate::month(i)),shrink = "none",window = window)
      results=ff_predict(model,future$data_matrix,groundtruth = future$groundtruth,templateraster = future$groundtruthraster,indices = future$testindices)
      writeRaster(results$predicted_raster,newfilename)
      if(analyse){ff_analyse(results$predicted_raster,future$groundtruthraster,csvfile = csvfile,append = append_to_csv,return_polygons = F)
      }
    }
  }
}




