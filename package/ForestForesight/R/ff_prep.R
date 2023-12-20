#' Prepare data for machine learning.
#'
#' This function prepares data for machine learning tasks based on specified parameters.
#'
#' @param datafolder Path to the data folder. Default is the system variable xgboost_datafolder. should contain the degrees folders
#' @param country Country for which the data is prepared. Is optional when tiles are given. Should be the ISO3 code.
#' @param tiles Vector of tiles in the syntax of e.g. 10N_080W.
#' @param groundtruth_pattern Pattern to identify ground truth files. 'groundtruth'.
#' @param start Start date for training data in the format c(YYYY, M). Default is c(2021, 1).
#' @param end End date for training data in the format c(YYYY, M). Default is NA to only process the start month.
#' @param inc_features Vector of included features. States which features to include in the data preparation.
#' @param exc_features Vector of excluded features. States which features to exclude in the data preparation.
#' @param fltr_features vector of features for filtering data. Default is 'forestmask2019'. needs to be combined with fltr_condition of the same length
#' @param fltr_condition Vector of filtering conditions. Default is '>0'. Should consist of operator and value and needs to be combined with fltr_features of same length vector
#' @param validation_sample float between 0 and 1 that indicates how much of the training dataset should be used for validation. Default is 0. Advised is to not set it above 0.3
#' @param sample_size Fraction size of the random sample. Should be bigger than 0 and smaller or equal to 1. Default is 1
#' @param relativedate Boolean indicating whether the date is relative. Default is \code{TRUE}.
#'
#' @return A prepared dataset for machine learning.
#' @export
#'
#'
#' @references
#' Jonas van Duijvenbode (2023)
#'
#' @keywords XGBoost data preparation
#' @rdname ff_prep
#' @name ff_prep


ff_prep=function(datafolder=NA,country=NA,tiles=NULL,groundtruth_pattern="groundtruth",start=c(2021,1),end=NA,inc_features=NA,exc_features=NA,fltr_features="forestmask2019",fltr_condition=">0",sample_size=1,validation_sample=0,relativedate=T,sampleraster=T,verbose=F){
  if(is.na(start[1])){stop("no start date given")}
  if(is.null(tiles)&is.na(country)){stop("unknown what to process since no tiles or country were given")}
  if(is.na(end[1])){end=start}
  if(is.na(datafolder)){datafolder=Sys.getenv("xgboost_datafolder")}
  if(datafolder==""){stop("No environment variable for xgboost_datafolder and no datafolder parameter set")}
  if(sampleraster&((validation_sample>0)|sample_size<1)){
    sampleraster=F
    warning("No template raster will be returned because the resulting matrix is sampled by either subsampling or validation sampling")}
  data(gfw_tiles)
  tilesvect=vect(gfw_tiles)
  if(!is.na(country)){
    data(countries)
    borders=vect(countries)
    tilesvect=tilesvect[borders[which(borders$iso3==country)]]$tile_id
    cat(paste("country contains the following tiles that will be processed:",paste(tilesvect,collapse=", "),"\n"))
    if(is.null(tiles)){tiles=tilesvect}
  }

  #list all the files for the tiles that have been selected
  allfiles=as.character(unlist(sapply(tiles,function(x) list.files(path=file.path(datafolder,x),full.names=T,recursive=T,pattern="tif$"))))
  if(length(allfiles)==0){stop(paste("no folders with tif-files found that correspond to the given tile id's:",paste(tiles,collapse=",")))}
  #remove features that are not wanted
  if(!is.na(exc_features[1])){
    exc_indices=unique(unlist(sapply(exc_features,function(x) which(startsWith(basename(allfiles),x)))))
    if(length(exc_indices)>0){allfiles=allfiles[-exc_indices]}}
  if(!is.na(inc_features[1])){
    inc_indices=unique(unlist(sapply(inc_features,function(x) which(startsWith(basename(allfiles),x)))))
    if(length(inc_indices>0)){allfiles=allfiles[inc_indices]}}
  if(length(allfiles)==0){stop("after including and excluding the requested variables there are no files left")}
  #create the range between start and end date
  daterange=as.character(seq(as.Date(paste0(start[1],"-",sprintf("%02d",start[2]),"-01")),as.Date(paste0(end[1],"-",sprintf("%02d",end[2]),"-01")),"1 month"))
  first=T
  if(length(tiles)>1){warning("No template raster will be returned because multiple tiles are processed together")
    sampleraster=F}
  for(tile in tiles){
    files=allfiles[grep(tile,allfiles)]
    static_files= files[-grep("01\\.",files)]
    #remove the loss files that would have predictive power
    if(length(grep("loss2020",static_files))>0){if(min(year(daterange))<2021){static_files=static_files[-grep("loss2020",static_files)]}}
    if(length(grep("loss2021",static_files))>0){if(min(year(daterange))<2022){static_files=static_files[-grep("loss2021",static_files)]}}
    if(length(grep("loss2022",static_files))>0){if(min(year(daterange))<2023){static_files=static_files[-grep("loss2022",static_files)]}}
    for(i in daterange){
      dynamic_files = sort(files[grep(i,files)])
      rasstack=c(rast(dynamic_files,win=ext(rast(static_files[1]))),rast(static_files,win=ext(rast(static_files[1]))))
      if(first){if(sampleraster){groundtruth_raster=rast(dynamic_files[grep(groundtruth_pattern,dynamic_files)])}else{groundtruth_raster=NA}}
      dts=as.matrix(rasstack)
      coords=xyFromCell(rasstack,seq(ncol(rasstack)*nrow(rasstack)))
      dts=cbind(coords,dts)
      if(relativedate){dts=cbind(dts,rep(sin((2*pi*as.numeric(format(as.Date(i),"%m")))/12),nrow(dts)))}
      dts[is.na(dts)]=0
      newcolnames=c("x","y",gsub(".tif","",c(sapply(basename(dynamic_files),function(x) strsplit(x,"_")[[1]][1]), basename(static_files))))
      if(relativedate){newcolnames=c(newcolnames,"sin_month")}
      colnames(dts)=newcolnames

      #take a random sample if that was applied
      if(sample_size<1){dts=dts[sample(seq(nrow(dts)),round(nrow(dts)*sample_size)),]}
      if(first){first=F;fdts=dts}else{
        common_cols <- intersect(colnames(dts), colnames(fdts))
        notin1=colnames(dts)[which(!(colnames(dts) %in% common_cols))]
        notin2=colnames(fdts)[which(!(colnames(fdts) %in% common_cols))]
        if(length(c(notin1,notin2))>0){warning(paste("the following columns are dropped because they are not present in the entire time series: ",paste(c(notin1,notin2),collapse=", ")))}
        # Subset matrices based on common column names
        # Merge matrices by column names
        fdts <- rbind(fdts[, common_cols, drop = FALSE], dts[, common_cols, drop = FALSE])
      }
    }
    if(verbose){cat(paste("features:",paste(newcolnames,collapse=", "),"\n"))}
  }
  #filter training data on features that have been declared
  filterindices=c()
  for(i in seq(length(fltr_features))){
    sf_indices=which(sapply(fdts[,which(colnames(fdts)==fltr_features[i])],function(x) eval(parse(text = paste(x, fltr_condition[i])))))
    filterindices=c(filterindices,sf_indices)
  }
  if(length(filterindices)>0){
    fdts=fdts[filterindices,]
  }
  #split data into feature data and label data
  groundtruth_index=which(colnames(fdts)==groundtruth_pattern)
  data_label=fdts[,groundtruth_index]
  #make sure that label data is binary
  data_label[data_label>1]=1
  fdts=fdts[,-groundtruth_index]

  if(validation_sample>0){
    sample_indices=sample(seq(nrow(fdts)),round(validation_sample*nrow(fdts)))
    data_matrix=xgb.DMatrix(fdts[-sample_indices,], label=data_label[-sample_indices])
    validation_matrix=xgb.DMatrix(fdts[sample_indices,], label=data_label[sample_indices])
  }else{
    data_matrix=xgb.DMatrix(fdts, label=data_label)
    validation_matrix=NA
  }

  return(list("data_matrix"=data_matrix,"validation_matrix"=validation_matrix,"testindices"=filterindices,"groundtruth"=data_label,"groundtruthraster"=groundtruth_raster,features=colnames(fdts)))
}

