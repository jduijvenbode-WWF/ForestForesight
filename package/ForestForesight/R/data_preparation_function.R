#' Prepare data for machine learning.
#'
#' This function prepares data for machine learning tasks based on specified parameters.
#'
#' @param datafolder Path to the data folder. Default is the system variable xgboost_datafolder. should contain the degrees folders
#' @param country Country for which the data is prepared. Is optional when tiles are given. Should be the ISO3 code.
#' @param countryfile Path to the country file. Not necessary if the file borders.geojson is in the datafolder.
#' @param tilesfile Path to the tiles file. Not necessary if the file integratedalerts.geojson is in the datafolder.
#' @param tiles Vector of tiles in the syntax of e.g. 10N_080W.
#' @param groundtruth_pattern Pattern to identify ground truth files. 'groundtruth'.
#' @param training_start Start date for training data in the format c(YYYY, M). Default is c(2021, 1).
#' @param training_end End date for training data in the format c(YYYY, M). Default is c(2022, 12).
#' @param test_month Test month for evaluation in the format c(YYYY, M). Example: c(2023,5).
#' @param inc_features Vector of included features. States which features to include in the data preparation.
#' @param exc_features Vector of excluded features. States which features to exclude in the data preparation.
#' @param fltr_features vector of features for filtering data. Default is 'forestmask2019'. needs to be combined with fltr_condition of the same length
#' @param fltr_condition Vector of filtering conditions. Default is '>0'. Should consist of operator and value and needs to be combined with fltr_features of same length vector
#' @param validation_sample float between 0 and 1 that indicates how much of the training dataset should be used for validation. Default is 0. Advised is to not set it above 0.3
#' @param random_sample_size Fraction size of the random sample. Should be bigger than 0 and smaller or equal to 1. Default is 1
#' @param apply_filter_on_test Boolean indicating whether to apply the filters in fltr_features also on test data. Default is TRUE.
#' @param relativedate Boolean indicating whether the date is relative. Default is \code{TRUE}.
#'
#' @return A prepared dataset for machine learning.
#' @export
#'
#' @seealso
#' \code{\link{other_function}}
#' \code{\link{another_function}}
#'
#' @references
#' Jonas van Duijvenbode (2023)
#'
#' @keywords XGBoost data preparation
#' @rdname data_preparation
#' @name data_preparation


data_preparation=function(datafolder=NA,country=NA,countryfile=NA,tilesfile=NULL,tiles=NULL,groundtruth_pattern="groundtruth",training_start=c(2021,1),training_end=c(2021,3),test_month,inc_features=NA,exc_features=NA,fltr_features="forestmask2019",fltr_condition=">0",validation_sample=0,random_sample_size=1,apply_filter_on_test=T,relativedate=T){
  library(terra);library(sf);library(lubridate)
  if(is.na(datafolder)){datafolder=Sys.getenv("xgboost_datafolder")}
  if(datafolder==""){stop("No environment variable for xgboost_datafolder and no datafolder parameter set")}
  data(gfw_tiles)
  tilesvect=vect(gfw_tiles)
  if(!is.na(country)){
    data(countries)
    borders=vect(countries)
    tilesvect=tilesvect[borders[which(borders$iso3==country)]]$tile_id
    if(is.null(tiles)){tiles=tilesvect}
    }

  #list all the files for the tiles that have been selected
  allfiles=as.character(unlist(sapply(tiles,function(x) list.files(path=file.path(datafolder,x),full.names=T,recursive=T,pattern="tif$"))))
  if(length(allfiles)==0){stop(paste("no folders with tif-files found that correspond to the given tile id's:",paste(tiles,collapse=",")))}
  #remove features that are not wanted
  #TODO sm6months would be removed if we would filter on 6months for instance
  if(!is.na(exc_features)){
    exc_indices=unique(unlist(sapply(exc_features,function(x) grep(x,allfiles))))
    if(length(exc_indices)>0){allfiles=allfiles[-exc_indices]}}
  if(!is.na(inc_features)){
    inc_indices=unique(unlist(sapply(inc_features,function(x) grep(x,allfiles))))
    if(length(inc_indices>0)){allfiles=allfiles[inc_indices]}}
  #create the range between start and end date
  training_daterange=as.character(seq(as.Date(paste0(training_start[1],"-",sprintf("%02d",training_start[2]),"-01")),as.Date(paste0(training_end[1],"-",sprintf("%02d",training_end[2]),"-01")),"1 month"))

  for(tile in tiles){
    files=allfiles[grep(tile,allfiles)]
    static_files= files[-grep("01\\.",files)]
    #remove the loss files that would have predictive power
    if(length(grep("loss2020",static_files))>0){if(min(year(training_daterange))<2021){static_files=static_files[-grep("loss2020",static_files)]}}
    if(length(grep("loss2021",static_files))>0){if(min(year(training_daterange))<2022){static_files=static_files[-grep("loss2021",static_files)]}}
    if(length(grep("loss2022",static_files))>0){if(min(year(training_daterange))<2023){static_files=static_files[-grep("loss2022",static_files)]}}
    test_month=as.character(as.Date(paste0(test_month[1],"-",sprintf("%02d",test_month[2]),"-01")))
    for(i in c(training_daterange,test_month)){
      dynamic_files = sort(files[grep(i,files)])
      rasstack=c(rast(dynamic_files,win=ext(rast(static_files[1]))),rast(static_files,win=ext(rast(static_files[1]))))
      dts=as.matrix(rasstack)
      coords=xyFromCell(rasstack,seq(ncol(rasstack)*nrow(rasstack)))
      dts=cbind(coords,dts)
      if(relativedate){dts=cbind(dts,rep(abs(round(as.numeric(as.Date(i))%%365.25)-183),nrow(dts)))}
      dts[is.na(dts)]=0
      newcolnames=c("x","y",gsub(".tif","",c(sapply(basename(dynamic_files),function(x) strsplit(x,"_")[[1]][1]), basename(static_files))))
      if(relativedate){newcolnames=c(newcolnames,"relativedate")}
      colnames(dts)=newcolnames
      #take a random sample if that was applied
      if(random_sample_size<1){dts=dts[sample(seq(nrow(dts)),round(nrow(dts)*random_sample_size)),]}
      if(i==test_month){testdts=dts}else{if(i==training_daterange[1]){traindts=dts}else{traindts=rbind(traindts,dts)}}
    }
  }
  #filter training data on features that have been declared
  for(i in seq(length(fltr_features))){
    traindts=traindts[which(sapply(traindts[,which(colnames(traindts)==fltr_features[i])],function(x) eval(parse(text = paste(x, fltr_condition[i]))))),]
  }
  #if applicable also filter the test data on the features that have been declared
  if(apply_filter_on_test){
    for(i in seq(length(fltr_features))){
      testfilterindices=which(sapply(traindts[,which(colnames(traindts)==fltr_features[i])],function(x) eval(parse(text = paste(x, fltr_condition[i])))))
    }
    if(exists("testfilterindices")){
      testdts=testdts[testfilterindices,]
    }
  }
  if(!exists("testfilterindices")){testfilterindices=NA}
  #split data into feature data and label data
  groundtruth_index=which(colnames(traindts)==groundtruth_pattern)
  train_label=traindts[,groundtruth_index]
  traindts=traindts[,-groundtruth_index]
  groundtruth_index=which(colnames(testdts)==groundtruth_pattern)
  test_label=testdts[,groundtruth_index]
  testdts=testdts[,-groundtruth_index]

  #make sure that label data is binary
  train_label[train_label>1]=1
  test_label[test_label>1]=1
  if(validation_sample>0){
    sample_indices=sample(seq(nrow(traindts)),round(validation_sample*nrow(traindts)))
    train_matrix=xgb.DMatrix(traindts[sample_indices,], label=train_label[sample_indices])
    validation_matrix=xgb.DMatrix(traindts[-sample_indices,], label=train_label[-sample_indices])
  }else{
    train_matrix=xgb.DMatrix(traindts, label=train_label)
    validation_matrix=NA
  }
  return(list("train_matrix"=train_matrix,"validation_matrix"=validation_matrix,
              "test_matrix"= xgb.DMatrix(testdts, label=test_label),"testindices"=testfilterindices,"groundtruth"=test_label))
}

