data_preparation=function(datafolder=NA,country=NA,countryfile=NA,tilesfile=NULL,tiles=NULL,groundtruth_pattern="groundtruth",training_start=c(2021,1),training_end=c(2022,12),test_month,inc_features=NULL,exc_features=NULL,fltr_features="forestmask2019",fltr_condition=">0",random_sample_size=1,apply_filter_on_test=T,relativedate=T){
  #folder should be a path to the folder that contains the 10-degree folders. Alternatively make sure your environment variable xgboost_datafolder is that folder
  #country should be a 3-letter iso code (Gabon=GAB,Laos=LAO). If this parameter is set, countryfile and tilefile should also be set
  #countryfile is only used when country is set and should contain a column called iso3.
  library(terra);library(sf);library(lubridate)
  if(is.na(datafolder)){datafolder=Sys.getenv("xgboost_datafolder")}
  curwd=getwd();setwd(datafolder)
  if(!is.na(country)){
    if(!is.na(countryfile)){borders=vect(sf::st_read(countryfile))}else{if(file.exists("borders.geojson")){borders=vect(sf::st_read("borders.geojson"))}else{break("no country file submitted and no borders.geojson in datafolder")}}
    if(!is.null(tilesfile)){tilesvect=vect(tilesfile)}else{if(file.exists("integratedalerts.geojson")){tilesvect=vect("integratedalerts.geojson")}else{break("no country file submitted and no integratedalerts.geojson in datafolder")}}
    covertiles=tilesvect[borders[which(borders$iso3==country)]]$tile_id
    if(is.null(tiles)){tiles=covertiles}
  }
  #list all the files for the tiles that have been selected
  allfiles=as.character(unlist(sapply(tiles,function(x) list.files(full.names=T,path=x,recursive=T,pattern="tif$"))))
  #remove features that are not wanted
  #TODO sm6months would be removed if we would filter on 6months for instance
  if(!is.null(exc_features)){allfiles=allfiles[-unique(unlist(sapply(exc_features,function(x) grep(x,allfiles))))]}
  if(!is.null(inc_features)){allfiles=allfiles[unique(unlist(sapply(inc_features,function(x) grep(x,allfiles))))]}
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
      #print(dynamic_files);print(static_files)
      rasstack=c(rast(dynamic_files,win=ext(rast(static_files[1]))),rast(static_files,win=ext(rast(static_files[1]))))
      dts=as.matrix(rasstack)
      coords=xyFromCell(rasstack,seq(ncol(rasstack)*nrow(rasstack)))
      filedate=substr(file,nchar(file)-13,nchar(file)-4)
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
  print(colnames(traindts))
  groundtruth_index=which(colnames(traindts)==groundtruth_pattern)
  print(groundtruth_index)
  train_label=traindts[,groundtruth_index]
  traindts=traindts[,-groundtruth_index]
  groundtruth_index=which(colnames(testdts)==groundtruth_pattern)
  test_label=testdts[,groundtruth_index]
  testdts=testdts[,-groundtruth_index]

  #make sure that label data is binary
  train_label[train_label>1]=1
  test_label[test_label>1]=1
  print("nondeju")
  #create a watchlist that can be used by xgboost
  print(nrow(traindts));print(length(train_label))
  print(nrow(testdts));print(length(test_label))
  watchlist = list(train = xgb.DMatrix(traindts, label=train_label), eval = xgb.DMatrix(testdts, label=test_label))
  #set the working directory back to original
  setwd(curwd)
  print("godver")
  return(list("watchlist"=watchlist,"testindices"=testfilterindices))
}
test=data_preparation(tiles="10N_080W",training_start=c(2022,01),training_end = c(2022,1),test_month = c(2022,9),random_sample_size = 0.6,exc_features = c("sm6months","loss2022"))
